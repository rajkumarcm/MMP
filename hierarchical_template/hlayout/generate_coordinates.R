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

nodes <- data.frame(id=1:15,
                    year=c(2010, 2011, 2011, 2012, 2012, 2012, 2012, 2013, 2013, 2013, 2013, 2014, 2013, 2014, 2014),
                    label=1:15)
edges <- data.frame(from=c(1,2,2,1,3,3,6,6,7,7,10,11,13,13),#,16,17),
                    to=c(2,4,5,3,6,7,8,9,10,11,12,12,14,15))#,17,18))

visited_nodes <- c()

fill_width <- function()
{
  for(i in 1:nrow(nodes))
  {
    node_id <- nodes[i, 'id']
    nodes[nodes$id==node_id, 'width'] <<- get_width(node_id)
  }
}

fill_width()

visited_nodes <- c()
node_spacing <- 90
global_prev_width <- 0
global_prev_x <- 0

compute_center <- function(current_width, prev_width, prev_x)
{
  current_center <- (max(1, current_width)*90)/2 - node_spacing/2
  unequal_divide_margin <- (prev_x+(node_spacing/2))+((prev_width*node_spacing)/2) # this should be zero for the left-most node in the graph
  margin_left <- unequal_divide_margin
  margin_left + current_center
}

estimate_xcoord <- function(node_id, nth_child, n_childs_parent, x, prev_width,
                            prev_x)
{
  # x -> parent's x corodinate
  # prev_x -> prev sibling's x coordinate
  if(! node_id %in% visited_nodes)
  {
    visited_nodes <<- c(visited_nodes, node_id)
    t_edges <- edges[edges$from==node_id, ]
    t_nodes <- unique(t_edges$to)
    n_childs <- length(t_nodes)
    # local_acc <- acc
    current_width <- nodes[nodes$id==node_id, 'width']
    current_x <- 0
    if(n_childs_parent > 1)
    {
      # current_center <- (max(1, current_width)*90)/2 - node_spacing/2
      # unequal_divide_margin <- (prev_x+(node_spacing/2))+((prev_width*node_spacing)/2) # this should be zero for the left-most node in the graph
      # margin_left <- unequal_divide_margin
      # nodes[nodes$id==node_id, 'x'] <<- margin_left + current_center
      # current_x <- margin_left + current_center
      current_x <- compute_center(current_width, prev_width, prev_x)
      nodes[nodes$id==node_id, 'x'] <<- current_x
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

    if(n_childs > 0)
    {
      for(i in 1:n_childs)
      {
        if(i > 1)
        {
          prev_child <- t_nodes[i-1]
          prev_x <- nodes[nodes$id==prev_child, 'x']
          prev_width <- nodes[nodes$id==prev_child, 'width']
          global_prev_width <<- prev_width
          global_prev_x <<- prev_x
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
  # else
  #   nodes[nodes$id==node_id, 'x']
}
#node_id, nth_child, n_childs_parent, x, prev_width, prev_x
# tmp <- estimate_xcoord(1, 1, 1, 225, 0, 0)

get_prev <- function(node_id)
{
  tmp_df <- data.frame(width=0, x=0)
  partition1 <- nodes[nodes$id < node_id,]
  if(nrow(partition1) > 0)
  {
    unique_years <- unique(partition1$year)
    min_year <- min(unique_years)
    prior_roots <- partition1[partition1$year==min_year, 'id']
    for(i in 1:length(prior_roots))
    {
      root_nid <- prior_roots[i]
      tmp_df$width <- tmp_df$width + nodes[nodes$id==root_nid, 'width']
    }
    
    #For x
    prior_roots <- partition1[partition1$year==min_year, 'x']
    tmp_df$x <- max(prior_roots)
  }
  tmp_df
}

fill_xcoord <- function()
{
  for(i in 1:nrow(nodes))
  {
    node_id <- nodes[i, 'id']
    current_width <- nodes[i, 'width']
    t_edges <- edges[edges$from==node_id, ]
    t_nodes <- unique(t_edges$to)
    n_childs <- length(t_nodes)
    
    prev <- get_prev(node_id)
    x_current <- compute_center(prev_x=prev$x, prev_width=prev$width,
                                current_width=current_width)
    estimate_xcoord(node_id=node_id, nth_child=1, n_childs_parent=1,
                    x=x_current, prev_width=prev$width, prev_x=prev$x)
  }
}

fill_xcoord()

adjust_coordinate <- function(edge)
{
  if(edge$to==12)
  {
    print('breakpoint...')
  }
  year <- edge$year
  to <- edge$to
  status <- edge$status
  from_nodes <- edges[edges$to==to, #&
                      #edges$year==year &
                      #edges$status==status,
                      'from']
  n_parents <- length(from_nodes)
  if(n_parents > 1)
  {
    parent_coordinates <- c()
    for(i in 1:length(from_nodes))
    {
      parent_coordinates <- c(parent_coordinates, 
                              nodes[nodes$id==from_nodes[i], 'x'])
    }
    min_coordinate <- min(parent_coordinates)
    max_coordinate <- max(parent_coordinates)
    width <- max_coordinate - min_coordinate
    centralised_coordinate <- min_coordinate + width/n_parents
    prev_coordinate <- nodes[nodes$id==edge$from, 'x']
    nodes[nodes$id==to, 'x'] <<- centralised_coordinate
    offset <- prev_coordinate - centralised_coordinate
    return(offset)
  }
  else
    return(0)
}

apply_offset <- function(node_id, offset)
{
  to_nodes <- edges[edges$from==node_id, 'to']
  if(length(to_nodes) > 0)
  {
    for(i in 1:length(to_nodes))
    {
      child_nid <- to_nodes[i]
      nodes[nodes$id==child_nid, 'x'] <<- nodes[nodes$id==child_nid, 'x'] + 
                                              offset
      apply_offset(child_nid, offset)
    }
  }
}

# Once X coordinates are generated, it is now time to ensure nodes that 
# have multiple parents get centralized.
visited_nodes <- c()
centralise_mpnodes <- function(node_id)
{
  if(! node_id %in% visited_nodes)
  {
    visited_nodes <- c(visited_nodes, node_id)
    to_edges <- edges[edges$from==node_id, ]
    if(nrow(to_edges) > 0)
    {
      for(i in 1:nrow(to_edges))
      {
        trailing_offset <- adjust_coordinate(to_edges[i,])
        if(to_edges[i, 'to'] == 12)
        {
          print('breakpoint...')
        }
        if(trailing_offset > 0)
        {
          apply_offset(to_edges[i, 'to'], trailing_offset)
        }
        centralise_mpnodes(to_edges[i, 'to'])
      }
    }
  }
}

centralise_mpnodes(1)

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