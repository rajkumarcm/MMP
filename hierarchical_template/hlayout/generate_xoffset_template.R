rm(list=ls())
library(dplyr)

get_width <- function(node_id)
{
  if(!node_id %in% visited_nodes)
  {
	  visited_nodes <<- c(visited_nodes, node_id)
	  t_edges <- edges[edges$from==node_id, ]
	  t_nodes <- unique(t_edges$to)
	  total_child_width <- 0
	  n_childs <- length(t_nodes)
	  
	  if(n_childs > 0)
	  {
  	  for(i in 1:length(t_nodes))
  	  {
    		child <- t_nodes[i]
    		tmp_width <- get_width(child)
    		nodes[nodes$id==child, 'width'] <<- tmp_width
    		total_child_width <- total_child_width + tmp_width
    		visited_nodes <<- c(visited_nodes, child)
  	  }
	  }
	  else
	    return(0) # the width would be 0 for the leaf node
		
	  max_width <- max(c(n_childs, total_child_width))
	  return(max_width)
	  
  }
  else
	nodes[nodes$id==node_id, 'width']
}

nodes <- data.frame(id=1:12,
                    year=c(2010, 2011, 2011, 2012, 2012, 2012, 2012, 2013, 2013, 2013, 2013, 2014))
edges <- data.frame(from=c(1,2,2,1,3,3,6,6,7,7,11),
					          to=c(2,4,5,3,6,7,8,9,10,11,12))

visited_nodes <- c()

fill_width <- function()
{
  for(i in 1:length(nodes))
  {
    node_id <- nodes[i, 'id']
    nodes[nodes$id==node_id, 'width'] <<- get_width(node_id)
  }
}

fill_width()


visited_nodes <- c()
node_spacing <- 90

estimate_xcoord <- function(node_id, nth_child, n_childs_parent, x, prev_width,
                            prev_x)
{
  # x -> parent's x corodinate
  # prev_x -> prev sibling's x coordinate
  if(! node_id %in% visited_nodes)
  {
    if(node_id==10)
    {
      print('breakpoint...')
    }
    visited_nodes <<- c(visited_nodes, node_id)
    t_edges <- edges[edges$from==node_id, ]
    t_nodes <- unique(t_edges$to)
    n_childs <- length(t_nodes)
    # local_acc <- acc
    current_width <- nodes[nodes$id==node_id, 'width']
    current_x <- 0
    if(n_childs_parent > 1)
    {
      current_center <- (max(1, current_width)*90)/2 - node_spacing/2
      # equal_divide_margin <- x - (node_spacing/2)
      unequal_divide_margin <- (prev_x+(node_spacing/2))+((prev_width*node_spacing)/2) # this should be zero for the left-most node in the graph
      # margin_left <- max(c(equal_divide_margin, unequal_divide_margin))
      margin_left <- unequal_divide_margin
      # margin_left <- equal_divide_margin
      # if(current_center==0)
      #   current_center <- node_spacing/2
      nodes[nodes$id==node_id, 'x'] <<- margin_left + current_center
      current_x <- margin_left + current_center
      
    }
    else if(nth_child==1 & n_childs_parent==1)
    {
      nodes[nodes$id==node_id, 'x'] <<- x
      current_x <- x
    }
    
    if(n_childs_parent > 1 & nth_child > 1)
    {
      nodes[nodes$id==node_id, 'x'] <<- nodes[nodes$id==node_id, 'x'] + 
                                            (node_spacing/2)
      current_x <- current_x + (node_spacing/2)
    }
    # else
    # {
    #   # When nth_child > 1 & n_childs_parent > 1
    #   
    # }
    
    # For the node that has children

    
    if(n_childs > 0)
    {
      for(i in 1:n_childs)
      {
        if(i > 1)
        {
          prev_child <- t_nodes[i-1]
          prev_x <- nodes[nodes$id==prev_child, 'x']
          prev_width <- nodes[nodes$id==prev_child, 'width']
        }
        
        child <- t_nodes[i]
        #node_id, nth_child, n_childs_parent, x, prev_x, acc)
        estimate_xcoord(child, i, n_childs, current_x, prev_width, prev_x)
        # local_acc <- local_acc + (current_width - max(c(1, current_width)))
      }
    }
    else{
      # Do not write anything here because
      # we do not need to encode any information about the main node here
      # as they are done before the beginning of the loop
    }
  }
  else
    nodes[nodes$id==node_id, 'x']
}

tmp <- estimate_xcoord(1, 1, 2, 360, 0, -node_spacing/2)

estimate_ycoord <- function(nodes)
{
  y.node_spacing <- 150
  years <- unique(nodes$year)
  years <- data.frame(year=years) %>% arrange(year)
  years$y <- seq(0, by=150, length.out=nrow(years))
  nodes <- nodes %>% inner_join(years, by="year")
  return(nodes)
}

nodes <- estimate_ycoord(nodes)
# save(file='nodes.RData', 'nodes')
# save(file='edges.RData', 'edges')