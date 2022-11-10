
#-----------------Hierarchical code---------------------------------------------

get_all_done <- function(nodes, edges)
{
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
  
  
  # Sometimes we need island nodes that would rather be on some side of the 
  # network to get squeezed
  # between nodes. This can be achieved using stabilization, but that's a very 
  # expensive operation. What is feasible and still guarantees correctness would 
  # be to trick the system to think that there is a connection from a node level
  # before the island node connecting "to" the island node. This would innately
  # make the system put the island node between the other nodes and once the
  # coordinates are generated, we can then remove the tricked edges so that false
  # connections do not show up, yet we have island nodes at the right place.
  # Nice trick huh?
  nodes$root <- F
  
  old_visited_nodes <<- c()
  visited_nodes <<- c()
  root_nodes <<- c()
  children <<- c()
  
  fill_width <- function()
  {
    for(i in 1:nrow(nodes))
    {
      node_id <- nodes[i, 'id']
      nodes[nodes$id==node_id, 'width'] <<- get_width(node_id)
      
      # If the DFS truly did discovered any new nodes
      if(length(visited_nodes) > length(old_visited_nodes))
      {
        root_nodes <- c(root_nodes, node_id)
        
        if(length(old_visited_nodes) > 0)
        {
          # Get the difference between previous visited nodes
          # and the current visited nodes
          new_children <- data.frame(id=visited_nodes) %>% 
            anti_join(data.frame(id=old_visited_nodes))
        }
        else
          new_children <- visited_nodes
        
        # Remove root node id from the newly visited nodes
        new_children <- new_children[new_children != node_id]
        
        nodes[nodes$id %in% new_children, 'root_id'] <<- node_id
        nodes[nodes$id == node_id, 'root'] <<- T
        old_visited_nodes <<- visited_nodes
      }
    }
  }
  
  fill_width()
  
  nodes$degree <- 0
  nodes$inc_intra_connections <- 0
  
  set_degree <- function(node_id)
  {
    connections <- edges[edges$from==node_id | edges$to==node_id, ]
    
    # degree = both incoming and outgoing
    nodes[nodes$id==node_id, 'degree'] <<- nrow(connections)
    
    # number of incoming intranet connections
    primary_root <- nodes[nodes$id==node_id, 'root_id']
    from_nodes <- edges[edges$to==node_id, 'from']
    root_of_connections <- nodes[nodes$id %in% from_nodes, 'root_id']
    inc_intra_connections <- sum(root_of_connections != primary_root)
    nodes[nodes$id==node_id, 'inc_intra'] <<- inc_intra_connections
  }
  
  get_degree <- function(node_id)
  {
    to_nodes <- edges[edges$from==node_id, 'to']
    n_childs <- length(to_nodes)
    
    if(n_childs > 0)
    {
      for(i in 1:n_childs)
      {
        child <- to_nodes[i]
        get_degree(child)
      }
    }
    set_degree(node_id)
  }
  
  fill_degree <- function()
  {
    root_nodes <- nodes[nodes$root==T, 'id']
    for(i in 1:length(root_nodes))
    {
      get_degree(root_nodes[i])
    }
  }
  
  fill_degree()
  
  # Mark clone nodes------------------------------------------------------------
  mark_clone <- function(tmp_nodes)
  {
    tmp_nodes$clone <- F
    labels <- unique(tmp_nodes[, 'label'])
    for(i in 1:length(labels))
    {
      # ids and years that correspond to the label
      corr_subset <- tmp_nodes[tmp_nodes$label == labels[i], c('id', 'year')]
      
      min_year <- min(corr_subset$year)
      tmp_nodes[tmp_nodes$label == labels[i] &
                  tmp_nodes$year > min_year, 'clone'] <- T
    }
    tmp_nodes
  }
  nodes <- mark_clone(nodes)
  nodes$color.background <- '#97C2FC'
  nodes[nodes$clone==T, 'color.background'] <- '#FB7E81'
  
  # #---------------------------------------------------------------------------
  
  visited_nodes <<- c()
  node_spacing <<- 90
  global_prev_width <<- 0
  global_prev_x <<- 0
  
  compute_center <- function(current_width, prev_width, prev_x)
  {
    current_center <- (max(1, current_width)*90)/2 - node_spacing/2
    
    # this should be zero for the left-most node in the graph
    unequal_divide_margin <- (prev_x+(node_spacing/2))+((prev_width*node_spacing)/2)
    margin_left <- unequal_divide_margin
    margin_left + current_center
  }
  
  argsort <- function(x)
  {
    indices <- c()
    for(i in 1:length(x))
    {
      tmp.min <- min(x, na.rm=T)
      index <- which(x==tmp.min)
      # Sometimes x could have same min value multiple times
      # that makes it produce multiple indices in index variable
      index <- index[1] # Just in case
      indices <- c(indices, index)
      x[index] <- NaN
    }
    indices
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
      
      merger_roots <- edges[edges$to==node_id &
                              edges$status=='Mergers', 'from']
      merger_roots <- merger_roots[merger_roots != node_id]
      merger_roots <- nodes[nodes$id %in% merger_roots & nodes$root==T, 'id']
      
      # # Rearrange the nodes here
      deg <- nodes[nodes$id %in% t_nodes, 'degree']
      indices1 <- which(deg == 1)
      indices2 <- which(deg > 1)
      indices <- c(indices1, indices2)
      t_nodes <- t_nodes[indices]
      
      # local_acc <- acc
      current_width <- nodes[nodes$id==node_id, 'width']
      current_x <- 0
      if(n_childs_parent > 1)
      {
        # When the parent has more than a single child
        current_x <- compute_center(current_width, prev_width, prev_x)
        nodes[nodes$id==node_id, 'x'] <<- current_x
      }
      else if(nth_child==1 & n_childs_parent==1)
      {
        # print(node_id)
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
      
      
      if(length(merger_roots) > 0)
      {
        for(i in 1:length(merger_roots))
        {
          if(merger_roots[i] == 688)
          {
            print('check if get_prev is working properly...')
          }
          prev <- get_prev(merger_roots[i])
          x_current <- compute_center(prev_x=prev$x, prev_width=prev$width,
                                      current_width=1)
          estimate_xcoord(merger_roots[i], 1, 1, x_current, prev$width, prev$x)
        }
      }
    }
    # else
    #   nodes[nodes$id==node_id, 'x']
  }
  
  get_prev <- function(node_id)
  {
    if(length(visited_nodes) == 0)
      return(data.frame(width=0, x=0))
    
    prev_roots <- unique(nodes[nodes$id %in% visited_nodes, 'root_id'])
    
    if(length(prev_roots) == 0)
      return(data.frame(width=0, x=0))
    
    last_root <- prev_roots[length(prev_roots)]
    last_x <- nodes[nodes$id==last_root, 'x']
    last_width <- nodes[nodes$id==last_root, 'width']
    last_x1 <- last_x + ((last_width * node_spacing)/2)
    
    if(length(visited_nodes) == 0)
      return(data.frame(width=0, x=0))
    
    year <- nodes[nodes$id == node_id, 'year']
    prev_nodes <- nodes[nodes$id %in% visited_nodes, ]
    adjacent_nodes <- prev_nodes[prev_nodes$year==year,]
    
    if(nrow(adjacent_nodes) == 0)
      return(data.frame(width=0, x=0))
    
    
    adjacent_nodes <- adjacent_nodes[!is.na(adjacent_nodes$x), ]
    last_x2 <- max(adjacent_nodes$x)
    
    last_x <- max(c(last_x1, last_x2))
    if(last_x == last_x2)
      last_width <- adjacent_nodes[adjacent_nodes$x==last_x, 'width'][1]
    
    prev <- data.frame(width=last_width, x=last_x)
  }
  
  
  fill_xcoord <- function()
  {
    root_nodes <- nodes[nodes$root==T, ]
    # browser()
    root_nodes <- root_nodes %>% arrange(year)
    prev_year <- unique(root_nodes$year)[1]
    
    # root_nodes <- root_nodes %>% arrange(desc(width))
    for(i in 1:nrow(root_nodes))
    {
      node_id <- root_nodes[i, 'id']
      current_width <- root_nodes[i, 'width']
      t_edges <- edges[edges$from==node_id, ]
      t_nodes <- unique(t_edges$to)
      n_childs <- length(t_nodes)
      
      if(node_id==624)
      {
        print('breakpoint at fill_xcoord. Inspect the value of prev and x_current to avoid overlap')
      }
      prev <- get_prev(node_id)
      
      
      x_current <- compute_center(prev_x=prev$x, prev_width=prev$width,
                                  current_width=current_width)
      estimate_xcoord(node_id=node_id, nth_child=1, n_childs_parent=1,
                      x=x_current, prev_width=prev$width, prev_x=prev$x)
    }
  }
  
  single_edge_node <- function(node_id)
  {
    node_connections <- edges[edges$from==node_id | edges$to==node_id,]
    return(nrow(node_connections)==1)
  }
  
  stabilise_graph <- function(node_id, n_childs_parent)
  {
    # n_childs_parent represents the number of childrens the parent node has
    to_nodes <- edges[edges$from==node_id, 'to']
    n_childs <- length(to_nodes)
    # When there child nodes
    if(n_childs > 0)
    {
      for(j in 1:n_childs)
      {
        child <- to_nodes[j]
        stabilise_graph(child, n_childs)
      }
    }
    else
    {
      # Leaf node
      # Check if this has any incoming edges that are not part of this 
      # isolated network
      from_nodes <- edges[edges$to==node_id, 'from']
      for(i in 1:length(from_nodes))
      {
        incoming_node <- from_nodes[i]
        if(single_edge_node(incoming_node) & n_childs_parent==1)
        {
          push_left(incoming_node, n_childs_parent)
        }
        
      }
    }
  }
  
  # Before we generate the x coordinates, it is imperative that we create 
  # tricked edges
  create_fake_edges <- function(nodes, edges)
  {
    edges$fake <- F
    root_nodes <- nodes[nodes$root==T, 'id']
    if(length(root_nodes) > 1)
    {
      for(i in 1:length(root_nodes))
      {
        node_id <- root_nodes[i]
        
        degree <- nodes[nodes$id==node_id, 'degree']
        if(degree == 1)
        {
          to_node <- edges[edges$from==node_id, 'to']
          prospective_root <- unique(nodes[nodes$id == to_node, 'root_id'])
          # current_root <- nodes[nodes$id==node_id, 'root_id']
          current_root <- node_id
          if(prospective_root != current_root)
          {
            # Then creating fake edge is a go
            
            # 1. Get the list of incoming connections this "to_node" has
            incoming_connections <- edges[edges$to==to_node,]
            from_nodes <- incoming_connections$from
            
            # 2. This could be one or many. If many, sort them by width
            # but before that first remove the current_root id from the 
            # from_nodes so we know what are the other nodes it receives
            # connections from.
            from_nodes <- from_nodes[from_nodes != current_root]
            
            # 3 Now sort by width
            # Yes I am deliberately overwriting this
            from_nodes <- edges[edges$to %in% from_nodes, 'from']
            if(length(from_nodes) > 0)
            {
              from_nodes <- nodes[nodes$id %in% from_nodes, c('id', 'root_id')]
              
              #argmax root width
              widths <- nodes[nodes$id %in% from_nodes$root_id, 'width']
              max_width <- max(widths)
              
              # corresponding root node to the max width
              corr_root_nodes <- nodes[nodes$root==T & 
                                         nodes$width==max_width, 'id']
              corr_from_node <- nodes[nodes$id %in% from_nodes & 
                                        nodes$root_id %in% corr_root_nodes, 'id']
              
              # Just in case corr_from_node has more than one, just select
              # the first one
              if(length(corr_from_node) > 1)
              {
                corr_from_node <- corr_from_node[1]
              }
              
              # All set. Create the fake edge now
              # First get a duplicate edge instead of having to create a 
              # dataframe with the same structure
              duplicate_edge <- edges[nrow(edges),]
              
              # Fake edge
              duplicate_edge$from <- corr_from_node
              duplicate_edge$to <- node_id
              duplicate_edge$fake <- T
              
              edges <- rbind(edges, duplicate_edge)
            }
          }
        }
      }
    }
    edges
  }
  
  # edges <- create_fake_edges(nodes, edges)
  
  cluster_nodes <- function(nodes)
  {
    root_nodes <- nodes[nodes$root==T, 'id']
    for(i in 1:length(root_nodes))
    {
      node_id <- root_nodes[i]
      degree <- nodes[nodes$id==node_id, 'degree']
      if(degree == 1)
      {
        to_node <- edges[edges$from==node_id, 'to']
        corr_root_id <- nodes[nodes$id==to_node, 'root_id']
        if(corr_root_id != node_id) # node_id = current root id
        {
          idx <- which(nodes$id==node_id)
          current_node <- nodes[idx,]
          nodes <- nodes[-idx,]
          corr_root_idx <- which(nodes$id==corr_root_id)
          nodes1 <- rbind(nodes[1:corr_root_idx,], current_node)
          nodes <- rbind(nodes1, nodes[(corr_root_idx+1):nrow(nodes),])
        }
      }
    }
    nodes
  }
  
  # nodes <- nodes %>% arrange(year, desc(width))
  nodes <- cluster_nodes(nodes)
  nodes$x <- 0
  fill_xcoord()
  
  adjust_coordinate <- function(edge)
  {
    
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
  visited_nodes <<- c()
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
          if(trailing_offset > 0)
          {
            apply_offset(to_edges[i, 'to'], trailing_offset)
          }
          centralise_mpnodes(to_edges[i, 'to'])
        }
      }
    }
  }
  
  centralise_all <- function()
  {
    root_nodes <- nodes[nodes$root==T, 'id']
    for(i in 1:length(root_nodes))
    {
      node_id <- root_nodes[i]
      centralise_mpnodes(node_id)
    }
  }
  # centralise_all()
  
  y.node_spacing <- 150
  estimate_ycoord <- function(nodes)
  {
    years <- unique(nodes$year)
    years <- data.frame(year=years) %>% arrange(year)
    years$y <- seq(0, by=y.node_spacing, length.out=nrow(years))
    nodes <- nodes %>% inner_join(years, by="year")
    return(nodes)
  }
  
  nodes <- estimate_ycoord(nodes)
  edges <- edges[edges$fake != T,]
  return(list(nodes, edges))
}


get_h_legend <- function(levels)
{
  # Here the levels is a dataframe that should contain both
  # the years and y-coordinate associated to the edge pertaining to that level
  
  new_levels <- NULL
  for(i in 1:(nrow(levels)-1))
  {
    prev_y <- levels[i, 'y']
    next_y <- levels[i+1, 'y']
    edge_y <- NA
    if(i == 1)
      edge_y <- prev_y + ((next_y - prev_y)/2) + 40 # 20 = offset
    else
      edge_y <- prev_y + ((next_y - prev_y)/2) + 70 # 20 = offset
    row <- data.frame(y=edge_y, year=levels[i+1, 'year'])
    
    if(is.null(new_levels))
    {
      new_levels <- row
    }
    else
    {
      new_levels <- rbind(new_levels, row)
    }
  }
  # browser()
  levels <- new_levels
  html_content <- ""
  for(i in 1:nrow(levels))
  {
    year <- levels[i, 'year']
    y <- levels[i, 'y']
    label <- as.character(year)
    line <- sprintf("<line x1='0' y1='%dpx' x2='40px' y2='%dpx' stroke='black' stroke-width='3px' />", y, y)
    text <- sprintf("<text x='50px' y='%dpx' class='legend_label'>%s</text>", y, label)
    sub_html <- paste0(line, text)
    html_content <- paste(html_content, sub_html)
  }
  html_content
}

#---------------End of hierarchical code----------------------------------------