n_nodes <- NULL
n_edges <- NULL

preprocess_hdata <- function(edges, old_nodes)
{
  edges <- edges %>% arrange(year)
  nodes <- NULL
  years <- unique(edges$year)
  count <- max(old_nodes$id)+1
  
  for(i in 1:length(years))
  {
    year <- years[i]
    tmp_edges <- edges[edges$year==year,]
    for(e in 1:nrow(tmp_edges))
    {
      edge <- tmp_edges[e,]
      link_id <- edge$link_id
      
      # Check if the node exist in appropriate years
      if(nrow(old_nodes[old_nodes$id==edge$from & 
                        old_nodes$year==(edge$year)-1, ])==0)
      {
        # Create fake node
        original_year <- old_nodes[old_nodes$id==edge$from, 'year']
        nodes <- rbind(nodes, data.frame(id=count, old_id=edge$from, 
                                         year=(edge$year)-1, 
                                         original_year=original_year))
        edge$from <- count
        count <- count + 1
      }
      
      # Check if the node exist in appropriate years
      if(nrow(old_nodes[old_nodes$id==edge$to & 
                        old_nodes$year==(edge$year)+1, ])==0)
      {
        # Create fake node
        original_year <- old_nodes[old_nodes$id==edge$to, 'year']
        nodes <- rbind(nodes, data.frame(id=count, old_id=edge$to, 
                                         year=(edge$year),
                                         original_year=original_year))
        edge$to <- count
        count <- count + 1
      }
      
      edges[edges$link_id==link_id,] <- edge
    }
  }
  
  edges <- edges %>% inner_join(old_nodes[, c('label', 'id')], by=c('from'='id'))
  cnames <- colnames(edges)
  cnames[cnames=='label'] <- 'group1_name'
  colnames(edges) <- cnames
  
  edges <- edges %>% inner_join(old_nodes[, c('label', 'id')], by=c('to'='id'))
  cnames <- colnames(edges)
  cnames[cnames=='label'] <- 'group2_name'
  colnames(edges) <- cnames
  nodes <- nodes %>% arrange(year)
  nodes$clone <- ifelse(nodes$year!=nodes$original_year, T, F)
  n_nodes <<- nodes
  n_edges <<- edges
}

load('IraqNodes.RData')
load('IraqEdges.RData')
preprocess_hdata(edges, nodes)
nodes <- n_nodes
edges <- n_edges
rm(list=c('n_nodes', 'n_edges'))