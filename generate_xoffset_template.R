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

nodes <- data.frame(id=1:12)
edges <- data.frame(from=c(1,2,2,1,3,3,6,6,7,7,10),
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
left_acc <- 0
node_spacing <- 30
estimate_xoffset <- function(node_id, acc)
{
  if(! node_id %in% visited_nodes)
  {
    visited_nodes <<- c(visited_nodes, node_id)
    t_edges <- edges[edges$from==node_id, ]
    t_nodes <- unique(t_edges$to)
    n_childs <- length(t_nodes)
    local_acc <- acc
    current_width <- nodes[nodes$id==node_id, 'width']
    
    if(n_childs > 0)
    {
      for(i in 1:n_childs)
      {
        child <- t_nodes[i]
        estimate_xoffset(child, local_acc)
        local_acc <- local_acc + (current_width - max(c(1, nodes[nodes$id==child, 'width'])))
      }
      # local_acc
      nodes[nodes$id==node_id, 'x'] <<- local_acc * node_spacing + 5
    }
    else{
      nodes[nodes$id==node_id, 'x'] <<- local_acc * node_spacing + 5
    }
  }
  else
    nodes[nodes$id==node_id, 'x']
}

tmp <- estimate_xoffset(1, 0)
