#---------------Load libraries-----------------------
library(shiny)
library(visNetwork)
library(networkD3)
library(RColorBrewer)
library(ggraph)
library(igraph)
library(shinyjs)
library(logging)
library(dplyr)
library(fuzzyjoin)
source('handle_data.R', local=T)
#---------------------------------------------------

# Variables definition------------------------------
# df -> edges data frame
# 
#---------------------------------------------------

#----------For debugging purposes-------------------
# 1. Check whether nodes$value is consumed - Fixed
# 2. Check the degreePal line where the degree histogram
# is discretized into 4 bins - Check the intervals of degree
# 3. Take a look at D1
# 4. Check why edges require information about node colors
# at line where df = merge(df, nodes) is called
#-- -- -- -- -- -- -- -- -- ---- -- -- -- ---- -- -- -- 
#- - - - - - - - - - - - -  - - - - - - - - - - - - - - 
# Update: I have commented line 126 for testing purposes.
# If anything breaks uncomment 126 and comment 127
#---------------------------------------------------

# Load and preprocess data--------------------------
df <- load_data()
df <- preprocess(df)
df <- remove_edges_rd(df)
df <- remove_edges_ry(df)
df <- make_undirected(df)
#---------------------------------------------------

# Check if this is needed-----------------------------------------
# gg <- make_graph(df)
#-----------------------------------------------------------------
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

# This would certainly cause confusion if left alive
rm('tmp_edges') 

# Nodes data frame
# Need to discuss about this with Iris. I don't completely agree with the
# way this has been created.
# The problem associated with this approach is, concatenated identifier
# is passed through unique function that makes it lose the order.
# When the order is lost, it is not possible to simply assign another column
# with information hoping they are relevant with each other.
# There is chance that they both cannot be related at all.
# This can cause nodes to carry wrong labels showing incorrect information.

# Reference link-------------------------------------------------------------
links <- read.csv("data/GroupLinks.csv", header=T)
# links$Anchor <- paste0('.*', paste0(links$Anchor, '.*'))

# Coordinates information----------------------------------------------------
nodes_extra <- read.csv("data/nodes_sample.csv", header=T,
                        colClasses = c("us_designated"="factor",
                                       "un_designated"="factor",
                                       "other_designated"="factor",
                                       "state_sponsor"="factor"))
extra_cnames <- data.frame(x=colnames(nodes_extra))


# Nodes database-------------------------------------------------------------

df_nodes <- read.csv("data/mmpgroupsfull.csv", header=T,)
# df_nodes <- df_nodes %>% regex_left_join(links[, c("URL", "Anchor")],
#                                          by=c("group_name"="Anchor"))
df_nodes <- df_nodes %>% left_join(links[, c("URL", "Anchor")],
                                   by=c("group_name"="Anchor"), keep=T)

df_nodes_cnames <- data.frame(x=colnames(df_nodes))
c_cnames <- extra_cnames %>% anti_join(df_nodes_cnames)
# c_cnames = complement column names in the nodes_sample.csv
c_cnames <- c_cnames$x

df_nodes <- df_nodes %>% left_join(nodes_extra[, c('group_id', c_cnames)], 
                                   by="group_id")
#------------------------------------------------------------------------------
# I need information on what shape to include ---------------------------------
# df_nodes$shape <- ifelse(df_nodes$us_designated==1 & df_nodes$state_sponsor)
#------------------------------------------------------------------------------
nodes = with(df, data.frame(id = unique(c(from, to))))
nodes <- nodes %>% inner_join(unique(df_nodes[, c("group_id", "group_name")]), 
                              by=c("id"="group_id"), keep=F)

colnames(nodes) <- c('id', 'label')
nodes$title <- nodes$label

# Centrality measures------------------------------------------------

# Create degree centrality
# Compute the number of incoming connection for each vertex
degree_value <- degree(graph, mode = "total")
nodes$value <- degree_value[match(nodes$id, names(degree_value))] + 1

# Throwing nodes into 5 different bins based on histogram binning technique
# on the basis of their strength represented by a unique color.
# But represented by just 4 colors for 5 bins ? I will change this to 4 for the 
# time-being
degreePal <- factor(cut(as.numeric(nodes$value), 4),
                    # After removing redundancies this code started giving trouble
                    # not allowing to have the following line
                    # labels = c("lightblue", "#619CFF", "orange", "darkblue"))
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
#c(0, 0.3, 10, 25, 
# 40, 60, 100, 1500,
# 6500)
betweenness_ranked <- as.numeric(cut(between, 
                                     breaks=c(0, 6e-4, 6e-3, 2e-01),
                                     include.lowest = T, 
                                     right=T, ordered_result = T))
# nodes$between_color <- c("slategrey", "gold", "tomato")[betweenness_ranked]
nodes$between_color <- c("#47AAF9", "#9351F3", "#F53030")[betweenness_ranked]
nodes$color.highlight.background <- c("#A2D1F7", "#B487F5", "#F88C8C")[betweenness_ranked]
nodes$color.hover.background <- c("#A2D1F7", "#B487F5", "#F88C8C")[betweenness_ranked]
nodes$color.border <- "slategrey"
nodes$color.hover.border <- "black"

nodes <- nodes[, c('id', 'value', 'central_color', 'between', 'between_color',
                   'color.border', 'color.highlight.background',
                   'color.hover.background', 'color.hover.border')]
# End of centrality---------------------------------------

# Merging edges with nodes dataframe to give more information to edges such as 
# degree of centrality,central_color, betweenness centrality and between_color.


df <- merge(df, nodes[, c('id', 'value')], by.x=c("from"), by.y=c("id"), all.x=T)

# Rename df_nodes columns
# df_nodes <- df_nodes[, c('group_id', 'group_name','description', 'startyear', 
#                          'map_name', 'lat', 'long', 'URL')]
# cnames <- c('id', 'label', 'title', 'level', 'map_name', 'latitude', 'longitude',
#             'URL')

cnames <- colnames(df_nodes)
cnames[cnames=='group_id'] <- 'id'
cnames[cnames=='group_name'] <- 'label'
cnames[cnames=='description'] <- 'title'
cnames[cnames=='startyear'] <- 'level'
# cnames[cnames=='lat'] <- 'latitude'
# cnames[cnames=='long'] <- 'longitude'
colnames(df_nodes) <- cnames

#------------------------------------------------------------------------------
# Since this is not a relationship dataframe, instead a database of nodes itself
# we want to ensure that there are no duplicates.
#------------------------------------------------------------------------------

df_nodes <- df_nodes[-which(df_nodes$map_name==""),]
df_nodes <- unique(df_nodes)

# Make this available for hierarchical plot code
status_dict <- unique(df[, c('status', 'status_id')])
status_dict <- status_dict[status_dict$status=='Mergers' | 
                             status_dict$status=='Splinters',]

coords <- read.csv('data/maps_coord.csv', header=T)
colnames(coords) <- c('map_name', 'latitude', 'longitude')

get_clusters <- function()
{
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
  
  tmp.edges <- unique(df[, c('from', 'to')] )
  tmp.edges <- tmp.edges %>% inner_join(df_nodes[, c('id', 'map_name')], 
                                        by=c('from'='id'), copy=T)
  cnames <- colnames(tmp.edges)
  cnames[cnames=='map_name'] <- 'g1_map'
  colnames(tmp.edges) <- cnames
  
  tmp.edges <- tmp.edges %>% inner_join(df_nodes[, c('id', 'map_name')], 
                                        by=c('to'='id'), copy=T)
  cnames <- colnames(tmp.edges)
  cnames[cnames=='map_name'] <- 'g2_map'
  colnames(tmp.edges) <- cnames
  
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
