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

# Nodes database-------------------------------------------------------------

df_nodes <- read.csv("data/mmpgroupsfull.csv", header=T,)

nodes = with(df, data.frame(id = unique(c(from, to))))

nodes <- nodes %>% inner_join(unique(df_nodes[, c("group_id", "group_name")]), 
                              by=c("id"="group_id"), keep=F)
# browser()
colnames(nodes) <- c('id', 'label')
nodes$title <- nodes$label

# Centrality measures------------------------------------------------

# Create degree centrality
# Compute the number of incoming connection for each vertex
degree_value <- degree(graph, mode = "in")
nodes$value <- degree_value[match(nodes$id, names(degree_value))]

# Throwing nodes into 5 different bins based on histogram binning technique
# on the basis of their strength represented by a unique color.
# But represented by just 4 colors for 5 bins ? I will change this to 4 for the 
# time-being
degreePal <- factor(cut(as.numeric(nodes$value), 4),
                    labels = c("lightblue", "#619CFF", "orange", "darkblue"))
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
nodes$between <- betweenness[match(nodes$id, names(betweenness))]
colorPalette <- colorRampPalette(c('blue','red'))

# After visualizing the distribution of betweenness centrality by means of
# boxplot the intervals in the breaks list was extracted.
#c(0, 0.3, 10, 25, 
# 40, 60, 100, 1500,
# 6500)
betweenness_ranked <- as.numeric(cut(nodes$between, breaks=c(0, 6e-4, 
                                                             6e-3, 2e-01)))
nodes$between_color <- colorPalette(3)[betweenness_ranked]

nodes <- nodes[, c('id', 'value', 'central_color', 'between', 'between_color')]
# End of centrality---------------------------------------

#------------------------------------------------------------------------------
# D1: For debugging purposes - total nodes in edges == nodes in nodes variable?
n_nodes1 <- length(unique(c(df$from, df$to)))
n_nodes2 <- length(unique(as.numeric(nodes$id)))
loginfo(paste('length of df$from:', n_nodes1))
loginfo(paste('length of nodes$id:', n_nodes2))
loginfo('Pay attention at the place where you merge df and nodes')
#--------------------------------------------------------------------------

# Merging edges with nodes dataframe to give more information to edges such as 
# degree of centrality,central_color, betweenness centrality and between_color.

#----- Update: df does not need information - central_color, between, and 
# between_color. Doesn't make any sense.
# df <- merge(df, nodes, by.x=c("from"), by.y=c("id"), all.x=T)
df <- merge(df, nodes[, c('id', 'value')], by.x=c("from"), by.y=c("id"), all.x=T)

# Rename df_nodes columns
df_nodes <- df_nodes[, c('group_id', 'group_name','description', 'startyear')]
cnames <- c('id', 'label', 'title', 'level')
colnames(df_nodes) <- cnames

#------------------------------------------------------------------------------
# Since this is not a relationship dataframe, instead a database of nodes itself
# we want to ensure that there are no duplicates.
#------------------------------------------------------------------------------
df_nodes <- unique(df_nodes)











