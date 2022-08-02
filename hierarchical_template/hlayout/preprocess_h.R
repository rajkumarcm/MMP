library(dplyr)
n_nodes <- NULL
n_edges <- NULL

get_nnode <- function(old_id, new_id, new_year, label, original_year, new_nodes, old_nodes)
{
  get_fake <- function(old_id, new_id, new_year, label, original_year, new_nodes, old_nodes)
  {
    # Create fake node
    new_node <- NULL
    if(nrow(old_nodes[old_nodes$id==old_id,])==1)
    {
      new_node <- old_nodes[old_nodes$id==old_id,]
    }
    else
    {
      # This must be a different edge, but corresponds to the fake node
      # that previous edge led to creating it.
      new_node <- new_nodes[new_nodes$id==old_id,]
    }
    new_node$id <- new_id
    new_node$year <- new_year
    new_node$original_id <- old_id
    new_node$original_year <- original_year
    return(rbind(new_nodes, new_node))
  }
  
  # Also make sure that new_nodes does not have what we are trying to replicate
  # at the respective year
  if(!is.null(new_nodes))
  {
    if(nrow(new_nodes[new_nodes$label==label & 
                      new_nodes$year==original_year]) == 1)
    {
      new_nodes
    }
    else if(nrow(new_nodes[new_nodes$label==label & 
                           new_nodes$year==original_year]) > 1)
    {
      print('get_nnode: Alert... more than one same node exist at the same level. Inspect')
      new_nodes
    }
    else
    {
      #old_id, new_id, new_year, label, original_year, new_nodes, old_nodes
      get_fake(old_id, new_id, new_year, label, original_year, new_nodes, old_nodes)
    }
  }
  else
  {
    #old_id, new_id, new_year, label, original_year, new_nodes, old_nodes
    get_fake(old_id, new_id, new_year, label, original_year, new_nodes, old_nodes)
  }
}

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
      old_edge <- tmp_edges[e,]
      new_edge <- old_edge
      if(old_edge$from==595 | old_edge$to==595)
      {
        # print('breakpoint at preprocess_hdata. Inspect why nodes 595 and 648 are missing')
      }
      
      # Check if the node exist in appropriate years - FROM
      if(nrow(old_nodes[old_nodes$label==old_edge$group1_name & 
                        old_nodes$year==(old_edge$year)-1, ])==0)
      {
        #old_id, new_id, new_year, label, original_year, new_nodes, old_nodes
        nodes <- get_nnode(new_edge$from, count, (old_edge$year)-1, 
                           old_edge$group1_name, (old_edge$year)-1, nodes, 
                           old_nodes)
        
        # Change the ID of the new edge
        new_edge$from <- count
        
        count <- count + 1
      }
      else
      {
        #old_id, new_id, new_year, label, original_year, new_nodes, old_nodes
        nodes <- get_nnode(new_edge$from, new_edge$from, (old_edge$year)-1, 
                           old_edge$group1_name, (old_edge$year)-1, nodes,
                           old_nodes)
      }
      
      # Check if the node exist in appropriate years - TO
      if(nrow(old_nodes[old_nodes$label==old_edge$group2_name & 
                        old_nodes$year==old_edge$year, ])==0)
      {
        # Create fake node
        #old_id, new_id, new_year, label, original_year, new_nodes, old_nodes
        nodes <- get_nnode(old_edge$to, count, old_edge$year, 
                           old_edge$group2_name, old_edge$year, nodes, old_nodes)
        
        # Change the ID of the new edge
        new_edge$to <- count
        
        count <- count + 1
        
        if(old_edge$status=='Splinters')
        {
          # If Splinters, then we must add another node that represents the
          # cloned node - these clone nodes specifically must not be marked
          # cloned as we are using the name clone for a different purpose
          # in other parts of the program.
          # Original use of term clone - node that was created in for instance
          # 1998, but when an edge goes from 2000 to 2001, then we will have to 
          # create
          # a clone node in 2000 so that we do not bring down an edge all the way
          # from 1998 thereby avoiding overlaps in the edges.
          
          #old_id, new_id, new_year, label, original_year, new_nodes, old_nodes
          nodes <- get_nnode(old_id=new_edge$from, new_id=count,
                             new_year=old_edge$year, 
                             label=old_edge$group1_name, 
                             original_year=old_edge$year, new_nodes=nodes,
                             old_nodes=old_nodes)
          
          # Also another edge pointing from the previous node to the newly
          # created clone node
          description <- paste("Part of",
                               paste(old_edge$group1_name, 
                                     "that splinted from the original group"))
          
          # second_edge represents the clone edge
          second_edge <- data.frame(from=new_edge$from, to=count, 
                                    link_id=NaN, old_link_id=NaN,
                                    multiple=old_edge$multiple,
                                    status='Splinters', year=old_edge$year,
                                    group1_name=old_edge$group1_name,
                                    group2_name=old_edge$group1_name,
                                    description=description,
                                    map_name=old_edge$map_name,
                                    primary=old_edge$primary,
                                    status_id=old_edge$status_id,
                                    title=description,
                                    color=old_edge$color,
                                    actor_color=old_edge$actor_color,
                                    value=old_edge$value,
                                    label=old_edge$group1_name)
          count <- count + 1
          
          edges <- rbind(edges, second_edge)
        }
      }
      else
      {
        #old_id, new_id, new_year, label, original_year, new_nodes, old_nodes
        nodes <- get_nnode(old_edge$to, old_edge$to, old_edge$year, 
                           old_edge$group2_name, old_edge$year, nodes, 
                           old_nodes)
        
        if(old_edge$status=='Splinters')
        {
          # If Splinters, then we must add another node that represents the
          # cloned node - these clone nodes specifically must not be marked
          # cloned as we are using the name clone for a different purpose
          # in other parts of the program.
          # Original use of term clone - node that was created in for instance
          # 1998, but when an edge goes from 2000 to 2001, then we will have to 
          # create
          # a clone node in 2000 so that we do not bring down an edge all the way
          # from 1998 thereby avoiding overlaps in the edges.
          
          #old_id, new_id, new_year, label, original_year, new_nodes, old_nodes
          nodes <- get_nnode(old_id=new_edge$from, new_id=count,
                             new_year=old_edge$year, label=old_edge$group2_name, 
                             original_year=old_edge$year, new_nodes=nodes,
                             old_nodes=old_nodes)
          
          # Also another edge pointing from the previous node to the newly
          # created clone node
          description <- paste("Part of",
                               paste(old_edge$group1_name, 
                                     "that splinted from the original group"))
          
          # second_edge represents the clone edge
          second_edge <- data.frame(from=new_edge$from, to=count, 
                                    link_id=NaN, old_link_id=NaN,
                                    multiple=old_edge$multiple,
                                    status='Splinters', year=old_edge$year,
                                    group1_name=old_edge$group1_name,
                                    group2_name=old_edge$group1_name,
                                    description=description,
                                    map_name=old_edge$map_name,
                                    primary=old_edge$primary,
                                    status_id=old_edge$status_id,
                                    title=description,
                                    color=old_edge$color,
                                    actor_color=old_edge$actor_color,
                                    value=old_edge$value,
                                    label=old_edge$group1_name)
          
          count <- count + 1
          
          edges <- rbind(edges, second_edge)
        }
        
      }
      
      # Modify the existing edge as to match the new node IDs
      edges[edges$from==old_edge$from &
            edges$to==old_edge$to &
            edges$year==old_edge$year &
            edges$status==old_edge$status,] <- new_edge
    }
  }
  
  edges <- edges %>% inner_join(nodes[, c('label', 'id')], by=c('from'='id'))
  cnames <- colnames(edges)
  cnames[cnames=='label'] <- 'group1_name'
  colnames(edges) <- cnames
  
  edges <- edges %>% inner_join(nodes[, c('label', 'id')], by=c('to'='id'))
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