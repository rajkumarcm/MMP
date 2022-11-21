#---------------Load libraries-----------------------
rm(list=ls())
library(shiny)
library(stringr)
library(visNetwork)
library(networkD3)
library(RColorBrewer)
library(ggraph)
library(igraph)
library(shinyjs)
library(logging)
library(dplyr)
library(fuzzyjoin)
library(leaflet)
library(tidyverse)
library(rlang)
# source('preproces_global.R', local=F)
source('handle_data.R', local=T)
map_idx <- 2
#---------------------------------------------------

# Variables definition------------------------------
# df -> edges data frame
# df_nodes -> nodes data frame
#---------------------------------------------------

# Load and preprocess data--------------------------
df <- load_edges_data()
df <- preprocess_df(df)
df <- remove_edges_rd(df)
df <- remove_edges_ry(df)
df <- make_undirected(df)
df$width <- 9

generate_node_properties <- function(df)
{
  tmp_edges <- data.frame(from = df$from, 
                          to = df$to, 
                          title=df$label,
                          group=df$status,
                          status=df$status, 
                          status_id=df$status_id, 
                          year=df$year, 
                          map=df$map,
                          color=df$color)
  
  graph <- graph.data.frame(tmp_edges, directed = T)
  
  nodes = data.frame(id=unique(c(df$from, df$to)))
  
  
  # Centrality measures------------------------------------------------
  
  # Create degree centrality
  # Compute the number of incoming connection for each vertex
  degree_value <- degree(graph, mode = "total")
  nodes$value <- degree_value[match(nodes$id, names(degree_value))] + 1
  # browser()
  
  # Throwing nodes into 5 different bins based on histogram binning technique
  # on the basis of their strength represented by a unique color.
  # But represented by just 4 colors for 5 bins ? I will change this to 4 for the 
  # time-being
  degreePal <- factor(cut(as.numeric(nodes$value), 4),
                      labels = c("lightblue", "#619CFF", "orange"))
  nodes$central_color <- degreePal
  # The amount of influence the vertex has on the flow of paths in the network
  # This should be somewhat an expensive operation.
  betweenness =  betweenness(graph, directed = F, normalized=T)
  
  # What is this doing is retrieving the betweenness centrality from
  # a complex dataframe that apparently has two values for each entry
  # one corresponding to the nodes$id and the other corresponds to the
  # betweenness centrality itself. Hence we use the match function to 
  # query the betweenness centrality value in the same order nodes are
  # organised in the nodes dataframe so we only link the values with those
  # that they truly belong to.
  between <- betweenness[match(nodes$id, names(betweenness))]
  nodes$between <- between
  colorPalette <- colorRampPalette(c('blue','red'))
  
  # After visualizing the distribution of betweenness centrality by means of
  # boxplot the intervals in the breaks list was extracted.
  
  betweenness_ranked <- as.numeric(cut(between, 
                                       breaks=c(0, 6e-4, 6e-3, 2e-01),
                                       include.lowest = T, 
                                       right=T, ordered_result = T))
  
  nodes$between_color <- c("#47AAF9", "#9351F3", "#F53030")[betweenness_ranked]
  nodes$color.highlight.background <- c("#A2D1F7", "#B487F5", "#F88C8C")[betweenness_ranked]
  nodes$color.hover.background <- c("#A2D1F7", "#B487F5", "#F88C8C")[betweenness_ranked]
  nodes$color.border <- "slategrey"
  nodes$color.hover.border <- "black"
  
  nodes <- nodes[, c('id', 'value', 'central_color', 'between', 'between_color',
                     'color.border', 'color.highlight.background',
                     'color.hover.background', 'color.hover.border')]
  nodes
}


# Nodes data frame

# Nodes database-------------------------------------------------------------
df_nodes <- load_nodes_data()
# browser()
profile_names <- unique(df_nodes$label)
profile_names <- profile_names[profile_names != "" & !is.na(profile_names)]
profile_names <- data.frame(label=profile_names) %>% arrange(label)
profile_names <- profile_names$label
# browser()

# nodes dataframe includes information about background.color,
# size of the node, etc.,
nodes <- generate_node_properties(df)
df_nodes.full <- df_nodes
df_nodes <- df_nodes %>% inner_join(nodes, by='id', keep=F)

maps <- c("All", unique(df$map_name))

# Make this available for hierarchical plot code
status_dict <- unique(df[, c('status', 'status_id')])
status_dict <- status_dict[status_dict$status=='Mergers' | 
                           status_dict$status=='Splinters',]

# Coordinates information for ggplot geography plot-----------------------------
coords <- read.csv('data/maps_coord.csv', header=T)
colnames(coords) <- c('hq_country', 'latitude', 'longitude')

remove_loop <- function(nodes, edges)
{
  map_names <- nodes$label
  rejection_list <- c()
  for(i in 1:length(map_names))
  {
    map_name <- map_names[i]
    indices <- which(edges$g1_map==map_name & edges$g2_map==map_name)
    # We do not want these edges
    if(length(indices) > 0)
    {
      rejection_list <- c(rejection_list, indices)
    }
  }
  edges <- edges[-rejection_list,]
}

remove_bidirection <- function(edges)
{
  accepted_list <- c()
  for(i in 1:nrow(edges))
  {
    edge <- edges[i,]
    if(edge$from==2 & edge$to==3)
    {
      # print('debug...')
      # browser()
    }
    
    reverse_edge_id <- edges[edges$from==edge$to &
                               edges$to==edge$from, 'id']
    count <- length(reverse_edge_id)
    if(count == 1)
    {
      # browser()
      if(!reverse_edge_id %in% accepted_list)
      {
        accepted_list <- c(accepted_list, edge$id)
      }
    }
    else if(count > 1)
    {
      print('catch me... this should not happen...')
    }
    else
      accepted_list <- c(accepted_list, edge$id)
  }
  
  edges <- edges[edges$id %in% accepted_list,]
}

get_clusters <- function()
{
  
  tmp.edges <- unique(df[, c('from', 'to')] )
  tmp.edges <- tmp.edges %>% inner_join(df_nodes[, c('id', 'hq_city')], 
                                        by=c('from'='id'), keep=F)
  # browser()
  cnames <- colnames(tmp.edges)
  cnames[cnames=='hq_city'] <- 'g1_map'
  colnames(tmp.edges) <- cnames
  
  tmp.edges <- tmp.edges %>% inner_join(df_nodes[, c('id', 'hq_city')], 
                                        by=c('to'='id'), keep=F)
  cnames <- colnames(tmp.edges)
  cnames[cnames=='hq_city'] <- 'g2_map'
  colnames(tmp.edges) <- cnames
  
  # browser()
  
  unique.edges <- unique(tmp.edges[, c('g1_map', 'g2_map')])
  tmp.nodes <- unique(c(unique.edges$g1_map, unique.edges$g2_map))
  tmp.nodes <- data.frame(id=seq(1, length(tmp.nodes)), label=tmp.nodes)
  
  unique.edges <- unique.edges %>% inner_join(tmp.nodes, by=c("g1_map"="label"))
  cnames <- colnames(unique.edges)
  cnames[cnames=='id'] <- 'from'
  colnames(unique.edges) <- cnames
  
  unique.edges <- unique.edges %>% inner_join(tmp.nodes, by=c("g2_map"="label"))
  cnames <- colnames(unique.edges)
  cnames[cnames=='id'] <- 'to'
  colnames(unique.edges) <- cnames
  
  unique.edges$id <- 1:nrow(unique.edges)
  # browser()
  unique.edges <- remove_loop(tmp.nodes, unique.edges)
  unique.edges <- remove_bidirection(unique.edges)
  
  tmp.nodes$shape <- 'database'
  tmp.nodes$color <- 'orange'
  tmp.nodes$font.size <- '28'
  return(list(tmp.nodes, unique.edges))
}

clustered_dfs <- get_clusters()
clustered_nodes <- clustered_dfs[[1]]
clustered_edges <- clustered_dfs[[2]]


# For Edit map UI conditional panel that is also shared with the server.
# 0 = edit maps, 1 = edit links, 2 = Manage Groups and Zoom levels
em_panel_sm <- T # edit maps
em_panel_mm <- F

years <- unique(df$year)
years <- data.frame(x = years) %>% arrange(x)
years <- years$x

data.files <- list.files('data/')
h_prof_file_found <- str_detect(data.files, 'hidden_profiles.RData')
h.profile_names <- NULL
if(sum(h_prof_file_found) == 1)
  load('data/hidden_profiles.RData')


provinces <- unique(df_nodes$hq_province)
provinces <- provinces[(!is.na(provinces)) & (provinces!="")]

valid.countries.list <- unique(df_nodes$hq_country)
valid.countries.list1 <- valid.countries.list[!is.na(valid.countries.list)]
valid.countries.list2 <- valid.countries.list1
valid.countries.list2[valid.countries.list2=='United States'] <- 'United States of America'
valid.countries.list2[valid.countries.list2=='New York'] <- 'United States of America'
valid.countries.list <- data.frame(hq_country_old=valid.countries.list1, 
                                   hq_country_new=valid.countries.list2)


# browser()
# loginfo('Finished global.R')

















