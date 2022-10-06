
# Note------------------------------------------------------------------------
# In the Sankey graph, connections are filtered to be either Splinters or 
# Mergers.
# dplyr::filter(status=="Splinters" | status == "Mergers")
# Why did they do this ?
#-----------------------------------------------------------------------------

library('maps')
library('geosphere')
library('ggplot2')
library('gridExtra')
library('tidyverse')
library('shinyjs')
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(sf)
library(tmap)
library(tmaptools)

source('filter_medges_all.R', local=T)
# source('generate_xoffset_template.R', local=T)
source('preprocess_h.R', local=T)

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

#---------------End of hierarchical code----------------------------------------
get_legend <- function(ledges, include_second_line=T)
{
   html_content <- ""
   y <- 30
   for(i in 1:nrow(ledges))
   {
     color <- ledges[i, 'color']
     label <- ledges[i, 'label']
     line1 <- sprintf("<line x1='0' y1='%dpx' x2='90px' y2='%dpx' stroke='%s' stroke-width='3px' />", y, y, color)
     text <- sprintf("<text x='100px' y='%dpx' class='legend_label'><a href='javascript:showDesc(\"%s\");'>%s</a></text>", y, label, label)
     line2 <- sprintf("<line x1='190px' y1='%dpx' x2='280px' y2='%dpx' stroke='%s' stroke-width='3px' />", y, y, color)
     sub_html <- ""
     if(include_second_line==T)
       sub_html <- paste0(line1, paste0(text, line2))
     else
       sub_html <- paste0(line1, text) 
     
     html_content <- paste(html_content, sub_html)
     y <- y + 50
   }
   html_content
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
    edge_y <- prev_y + ((next_y - prev_y)/2) + 20 # 20 = offset
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

#-----------------Code that used to be inside nodes reactive block--------------

get_nodes <- function(edges.df)
{
  # browser()
  node.size.offset <- 0
  map_name <- unique(edges.df$map_name) # This should return only 1 value
  if(length(map_name) > 1)
  {
    map_name <- 'All'
  }
  tmp_df <- data.frame(id = unique(c(edges.df$from,
                                     edges.df$to)))
  tmp_df <- tmp_df %>% inner_join(unique(df_nodes[df_nodes$map_name==map_name | 
                                                  map_name=='All', 
                                                  c('id', 'label', 'title', 
                                                      'level', 'shape', 
                                                    'un_designated', 
                                                    'us_designated',
                                                    'state_sponsor',
                                                    'other_designated')]),
                                   by="id", keep=F)
  
  # nodes dataframe is created using correct inner join
  tmp_df <- merge(tmp_df, nodes[, c('id', 'between_color', 'value', 
                                    'color.border', 'color.highlight.background',
                                    'color.hover.background',
                                    'color.hover.border')], 
                  by='id')
  cnames <- colnames(tmp_df)
  cnames[cnames == 'between_color'] <- 'color.background'
  colnames(tmp_df) <- cnames
  tmp_df$value <- tmp_df$value + node.size.offset
  tmp_df
}

#----------------Code that inside renderVisNetwork for Spatial graph------------

get_spatial_visNetwork <- function(nodes, edges)
{
  legend.df <- data.frame(shape=unique(nodes$shape))
  legend.df$label <- ""
  legend.df[legend.df$shape=='star', 'label'] <- 'US or UN'
  legend.df[legend.df$shape=='diamond', 'label'] <- 'Only US'
  legend.df[legend.df$shape=='triangle', 'label'] <- 'US or UN, and State Sponsored'
  legend.df[legend.df$shape=='square', 'label'] <- 'None'
  # browser()
  
  visNetwork(nodes,
             edges,
             width = "100%")  %>%
    visLegend(position='right', main='Legend', 
              addNodes=legend.df, stepY=150, stepX=150) %>%
    visEvents(type='on',
              select = "function(properties) {
     Shiny.setInputValue('link_nid', properties.nodes);}",
              stabilizationProgress = "function(params){
                    Shiny.setInputValue('updatePB', params);
                }",
              stabilized = "function(params){
                    Shiny.setInputValue('completePB', params);
                }",
              zoom = "function(properties){
                    Shiny.setInputValue('clusterNodes', properties);
                }"
    ) %>%
    visPhysics(solver = "forceAtlas2Based",
               forceAtlas2Based = list(avoidOverlap=0.7,
                                       gravitationalConstant=-100,
                                       damping=1)
    ) %>%
    
    #-------------------TEMPORARILY DISABLED AS JS IS BREAKING----------------      
  visNodes(shadow=T,
           borderWidth = 2,#TEMPORARILY DISABLED
           # borderWidthSelected = 3, TEMPORARILY DISABLED
           color=list(hover=list(border='tango'#,
                                 # borderWidth=3
           )))  %>%
    visEdges(
      label=edges()$title,
      font = list(size = 2)) %>%
    visInteraction(tooltipStyle = 'position: fixed;visibility:hidden;padding: 5px;
                font-family: verdana;font-size:14px;font-color:#000000;background-color: #f5f4ed;
                -moz-border-radius: 3px;-webkit-border-radius: 3px;border-radius: 3px;
                 border: 1px solid #808074;white-space: wrap;box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2);
                 max-width:300px',
                   hover = TRUE,
                   keyboard = TRUE,
                   dragNodes = T,
                   dragView = T,
                   zoomView = T,
                   multiselect = T) %>%   # explicit edge options
    visOptions(
      highlightNearest = list(enabled=T, #hover=T,
                              algorithm="hierarchical",
                              degree=list(from=0, to=2)),
      nodesIdSelection = TRUE,
      autoResize = T) %>%
    visExport()
}

filter_profiles <- function(edges)
{
  indices <- which(edges$group1_name %in% h.profile_names | 
                   edges$group2_name %in% h.profile_names)
  edges <- edges[-indices,]
  
  edges
}

get_activeg_df <- function(df_nodes, map_name)
{
  tmp.nodes <- unique(df_nodes[df_nodes$map_name==map_name, 
                               c('id', 'label', 'level', 'active')])
  cnames <- colnames(tmp.nodes)
  cnames[cnames == 'level'] <- 'year'
  colnames(tmp.nodes) <- cnames
  tmp.nodes <- tmp.nodes %>% filter(active == 1) %>% arrange(year)
  tmp.nodes <- tmp.nodes %>% group_by(year) %>% 
    summarise(n_active_groups=n())
}

fill_activeg_years <- function(tmp.df)
{
  min.year <- min(tmp.df$year)
  max.year <- max(tmp.df$year)
  years <- seq(min.year, max.year, 1)
  for(i in 1:length(maps))
  {
    mname <- maps[i]
    for(j in 1:length(years))
    {
      year <- years[j]
      if(!exists(tmp.df[tmp.df$year==year & tmp.df$map_name==mname, 
                       c('label')]))
      {
        tmp.df <- rbind(tmp.df, 
                        data.frame(map_name=mname, year=year, n_active_groups=0))
      }
    }
  }
  tmp.df
}

filter_hidden_profiles <- function(edges)
{
    edges <- edges %>% filter((! group1_name %in% h.profile_names) & 
                              (! group2_name %in% h.profile_names))
    edges
}

filter_designation <- function(nodes, selected_desig)
{
  filt.nodes <- nodes # DELETE AFTER DEBUGGING------\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
  # browser()
  if((!"All" %in% selected_desig) | length(selected_desig)==0)
  {
    # browser()
    filt.nodes <- NULL
    if("US" %in% selected_desig)
      filt.nodes <- rbind(filt.nodes, nodes %>% filter(us_designated==1))
    if("UN" %in% selected_desig)
      filt.nodes <- rbind(filt.nodes, nodes %>% filter(un_designated==1))
    if("State" %in% selected_desig)
      filt.nodes <- rbind(filt.nodes, nodes %>% filter(state_sponsor==1))
    if("Others" %in% selected_desig)
      filt.nodes <- rbind(filt.nodes, nodes %>% filter(other_designated==1))
    
    filt.nodes <- unique(filt.nodes)
  }
  filt.nodes
}

#---------------------------SERVER CODE-----------------------------------------

# House Keeping Parameters---------------
mm_map_name <- ''
animate <- F
# df_nodes.copy <- unique(df_nodes[, c('id', 'label', 'level', 'title', 'active', 
#                                      'shape', 'us_designated', 'URL', 'endyear')]) %>%
#                       arrange(label) %>% filter(label != "")
df_nodes.copy <- unique(df_nodes[, c('id', 'label', 'level', 'active', 
                                          'URL', 'endyear')]) %>%
                 arrange(label) %>% filter(label != "")
df_nodes.copy.original <- df_nodes.copy
df.copy <- df
avoidLB <- F
ep_changes_made <- F
d.profile_names <- NULL
#----------------------------------------

s <- shinyServer(function(input, output, session){
  
  shinyjs::onclick('toggleMenu', shinyjs::showElement(id='sp', anim=T, animType='fade'))
  shinyjs::onclick('closeSp', shinyjs::toggle(id='sp', anim=T, animType='fade'))
  
  # cloned version of the original df
  # This was done in order to faciliate the process of stopping the progress bar
  # from loading when the timeline slider is tweaked. Because stabilized event
  # by as a bug is failing to fire when the network reaches the stabilized point.  
  initial_run <- T
  minYr <- min(df$year)
  maxYr <- max(df$year)

  filtered_df <- reactive({
    prev.map_name <- maps[map_idx]
    if(prev.map_name == input$map_name & initial_run == F)
    {
      avoidLB <<- T
    }
    else
    {
      avoidLB <<- F
      initial_run <<- F
      prev.map_name <<- input$map_name
    }
    tmp_df <- df %>% 
      filter(input$map_name  == 'All' | map_name == input$map_name)
    
    if(input$map_name=='All')
      tmp_df <- filter_edges_mmap(tmp_df)
    
    tmp_df <- filter_hidden_profiles(tmp_df)
    tmp_df
  })
  
  edges <- reactive({
    
    # When edges do not have a unique identifier, then visnetwork
    # cannot be asked to remove or add them back to the network. Any form
    # of manipulation that happens without unique ID will be erroneous. 
    # I have included an ID that made the status filter to now work under both
    # map=All and map=any map_name
    data.frame(
               id = filtered_df()$link_id,
               from = filtered_df()$from, 
               to = filtered_df()$to,
               source = as.character(filtered_df()$group1_name),
               target = as.character(filtered_df()$group2_name),
               title=filtered_df()$title,
               status_id=filtered_df()$status_id, 
               year=filtered_df()$year, 
               map_name=filtered_df()$map_name,
               color=filtered_df()$color
    )
  })
  
  # Once again, as I mentioned earlier in one of the places where nodes
  # data frame was originally created, I do not agree with the way id and
  # label attributes are associated given unique function is used that will
  # perhaps erase the order. There are chances that one relationship is
  # given label of another relationship.
  nodes2 <- reactive({
    tmp.nodes <- get_nodes(filtered_df())
    # tmp.nodes <- filter_designation(tmp.nodes, input$filterDesig)
  })

  # edges data.frame for legend
  # browser()
  tmp_df <- unique(df[, c('status', 'color')])
  
  f_years <- reactive({
    years <- unique(edges()$year)
    years <- data.frame(year=years) %>% arrange(year)
    years <- years$year
    years
  })
  
  
  # ledges is created & used for the sake of displaying legend
  # that aid in understanding the edges
  ledges <- data.frame(color = tmp_df$color,
                       label = tmp_df$status
  )

  output$networkvisfinal <- renderVisNetwork({
      updateSliderInput(session, 'range',
                        min=min(edges()$year), max=max(edges()$year),
                        value=c(min(edges()$year), max(edges()$year))
      )
      
      # Also update the options under sponsor filter
      tmp.nodes <- nodes2()
      checkbox_choices <- c('All')
      if(1 %in% unique(tmp.nodes$us_designated))
        checkbox_choices <- c(checkbox_choices, 'US')
      if(1 %in% unique(tmp.nodes$un_designated))
        checkbox_choices <- c(checkbox_choices, 'UN')
      if(1 %in% unique(tmp.nodes$state_sponsor))
        checkbox_choices <- c(checkbox_choices, 'State')
      if(1 %in% unique(tmp.nodes$other_designated))
        checkbox_choices <- c(checkbox_choices, 'Others')
      
      updateCheckboxGroupInput(session, inputId='filterDesig', 
                               label='Filter Designation',
                               choices=checkbox_choices,
                               selected=checkbox_choices, inline=T
                               )
    
    
    get_spatial_visNetwork(nodes2(), edges())
  })
  
  myVisNetworkProxy <- visNetworkProxy("networkvisfinal")
  
  observeEvent(input$filterDesig, {
    if(is.null(input$filterDesig))
    {
      session$sendCustomMessage('invalid_operation', 'empty graph')
    }
    else
    {
      filt.nodes <- filter_designation(nodes2(), input$filterDesig)
      removed.nodes <- nodes2() %>% anti_join(filt.nodes, by='id')
      visRemoveNodes(myVisNetworkProxy, removed.nodes$id)
      visUpdateNodes(myVisNetworkProxy, filt.nodes)
      
      # Want to ensure edges that still have entries with previously removed
      # nodes now get activated, if included after the filter, by updating once
      # again.
      visUpdateEdges(myVisNetworkProxy, edges())
    }
  }, ignoreNULL=F)
  
  output$year_slider <- renderPrint({
    html.outer <- "<div id='years_list_container'><div id='year_list_sub_container'>"
    html.inner <- ""
    for(i in 1:length(f_years()))
    {
      html.inner <- paste0(html.inner, sprintf("<div class='year_field'>%d</div>", f_years()[i]))
    }
    scriptContent <- "<script>
                       yrsc = document.getElementById('year_list_sub_container');
                       yrsc.style.marginTop = '9px';
                      </script>"
    htmlComplete <- paste0(html.outer, paste0(html.inner, paste0("</div></div>", scriptContent)))
    
    cat(htmlComplete)
  })
  
  observeEvent(input$range, {
    filtered.edges <- edges() %>% filter(year >= input$range[1] & 
                                         year <= input$range[2])
    filtered.nodes <- get_nodes(filtered.edges)
    complement.edges <- edges() %>% anti_join(filtered.edges, by='id')
    complement.nodes <- get_nodes(complement.edges)
    
    # Update visNetwork once the MAIN year slider is updated
    # Note: This has nothing to do with the animation or the year slider
    # associated with animation.
    visRemoveEdges(myVisNetworkProxy, complement.edges$id)
    visRemoveNodes(myVisNetworkProxy, complement.nodes$id)
    visUpdateNodes(myVisNetworkProxy, filtered.nodes)
    visUpdateEdges(myVisNetworkProxy, filtered.edges)
  })
  
  #------------Have to duplicate call to visNetwork for Animation---------------
  observeEvent(input$animateBtn, {
    
    logging::loginfo(input$animate_spatial)
    
    # browser()
    tmp.edges <- edges()
    tmp.edges <- tmp.edges %>% filter(year <= f_years()[1])
    tmp.nodes <- get_nodes(tmp.edges)
    if(input$animate_spatial==T)
    {
      # browser()
      visRemoveEdges(myVisNetworkProxy, edges()$id)
      visRemoveNodes(myVisNetworkProxy, nodes2()$id)
      visUpdateNodes(myVisNetworkProxy, tmp.nodes)
      visUpdateEdges(myVisNetworkProxy, tmp.edges)
      Sys.sleep(7)
      
      for(i in 2:length(f_years()))
      {
        # browser()
        session$sendCustomMessage('tmpAnimate', i)
        Sys.sleep(2)
        tmp.edges <- edges()
        tmp.edges <- tmp.edges %>% filter(year <= f_years()[i])
        tmp.nodes <- get_nodes(tmp.edges)
        visUpdateNodes(myVisNetworkProxy, tmp.nodes)
        visUpdateEdges(myVisNetworkProxy, tmp.edges)
        if(i != length(f_years()))
          Sys.sleep(7) # Year transition delay
        else
          Sys.sleep(4)
      }
    }
    
    session$sendCustomMessage('resetYearAnSlider', '')
  })
  
  #-----------------------------------------------------------------------------
  
  
  
  observeEvent(input$updatePB, {
    if(!avoidLB)
      session$sendCustomMessage('updatePB', input$updatePB)
  })
  
  observeEvent(input$completePB, {
    if(!avoidLB)
    {
      params <- NULL
      params$iterations <- 1000
      session$sendCustomMessage('completePB', params)
    }
  })
  
  alreadyClustered <- F
  observeEvent(input$clusterNodes, {
    if(input$clusterNodes[['scale']] < 0.08 & alreadyClustered==F & 
       input$map_name=='All')
    {
      # browser()
      session$sendCustomMessage('toggleSL', '')
      alreadyClustered <<- T
      visRemoveEdges(myVisNetworkProxy, edges()$id)
      visRemoveNodes(myVisNetworkProxy, nodes2()$id)
      visUpdateNodes(myVisNetworkProxy, clustered_nodes)
      visUpdateEdges(myVisNetworkProxy, clustered_edges)
    }
    else if(input$clusterNodes[['scale']] >= 0.6 & alreadyClustered==T & 
            input$map_name=='All')
    {
      alreadyClustered <<- F
      session$sendCustomMessage('toggleSL', '')
      visRemoveEdges(myVisNetworkProxy, clustered_edges$id)
      visRemoveNodes(myVisNetworkProxy, clustered_nodes$id)
      visUpdateNodes(myVisNetworkProxy, nodes2())
      visUpdateEdges(myVisNetworkProxy, edges())
    }
  })
  
  observeEvent(input$link_nid, {

    url <- unique(df_nodes[df_nodes$id==input$link_nid & 
                      (df_nodes$map_name==input$map_name | input$map_name=='All'), "URL"])
    # browser()
    gname <- unique(df_nodes[df_nodes$id==input$link_nid & 
                        (df_nodes$map_name==input$map_name | input$map_name=='All'), "label"])
    if(is.na(url))
    {
      url <- "#"
    }else if (length(url)==0)
    {
      url <- "#"
    }
    output$link <- renderUI({
      HTML(sprintf("<a href='%s'>%s</a>", url, gname))
    })
  })
  
  observeEvent(input$inputGN, {
    # logging::loginfo(input$inputGN)
    output$gn_list <- renderPrint({
      groups <- unique(nodes2()[, c('id', 'label')])
      # groups <- data.frame(group=groups) %>% arrange(group)
      # groups <- groups$group
      groups <- groups %>% arrange(label)
      boolean_mask <- str_detect(groups$label, sprintf('\\w*%s\\w*', input$inputGN))
      matching_groups <- groups[boolean_mask, ]

      if(sum(boolean_mask) > 0)
      {
        html_wrapper_start <- "<div id='gn_list_sub'>"
        html_list <- ""
        for(i in 1:nrow(matching_groups))
        {
          group.name <- matching_groups[i, 'label']
          group.id <- matching_groups[i, 'id']
          html_list <- paste0(html_list, HTML(sprintf("<div class='gn_item'><a href=\"javascript:Shiny.setInputValue('sNFromList', %d);\">%s</a></div>", group.id, group.name)))
        }
        html_wrapper_end <- "</div>"
        group_list.html <- paste0(html_wrapper_start, paste0(html_list,
                                                              html_wrapper_end))
        cat(group_list.html)
      }
    })
  })
  
  
  observeEvent(input$sNFromList, {
    visSelectNodes(myVisNetworkProxy, input$sNFromList)
  })
  
  # This observe updates the nodes and edges on visnetworkfinal - the main
  # spatial graph
  observe ({
    filteredEdges <- edges()[edges()$status_id %in% as.numeric(input$filterEdges), , drop = FALSE]
    filteredNodes2 <- data.frame(id=unique(c(filteredEdges$from,
                                             filteredEdges$to)))
    
    filteredNodes2 <- filteredNodes2 %>% inner_join(df_nodes[, c("id", "label")],
                                                    by="id", keep=F)
    filteredNodes2 <- filteredNodes2 %>% inner_join(nodes, by='id', keep=F)
    cnames <- colnames(filteredNodes2)
    cnames[cnames == 'between_color'] <- 'color.background'
    colnames(filteredNodes2) <- cnames
    
    hiddenNodes <- anti_join(nodes2(), filteredNodes2)
    hiddenEdges <- anti_join(edges(), filteredEdges)

    visRemoveNodes(myVisNetworkProxy, id = hiddenNodes$id)
    visRemoveEdges(myVisNetworkProxy, id=hiddenEdges$id)
    visUpdateNodes(myVisNetworkProxy, nodes = filteredNodes2)
    # Should update the edges when changes are made. visUpdateNodes is not 
    # adequate
    visUpdateEdges(myVisNetworkProxy, edges=filteredEdges)

  })
  
  # This observes the status checkboxes input and as soon as the status
  # is checked or unchecked - For the highlight status
  observeEvent(input$filterEdges, {
    # Update the input list that shows the available edges to be selected
    # We cannot show all edges after input$filterEdges has been altered.
    # All edges will only make sense to be shown when no status in the checkbox
    # is left unchecked.
    default_choices <- c("0"="None",
                         "5"="Affiliates", 
                         "2"="Allies", 
                         "3"="Mergers",
                         "1"="Rivals",
                         "4"="Splinters")
    availableRel <- input$filterEdges
    filtered_choices <- c()
    filtered_choices[["None"]] <- 0
    for(i in 1:length(availableRel))
    {
      choice_id <- availableRel[i]
      choice <- default_choices[choice_id]
      filtered_choices[[choice]] <- as.numeric(choice_id)
    }
    updateSelectInput(session, 'selectStatus', label="Highlight one status",
                      choices=filtered_choices)
  })
  
  observeEvent(input$selectStatus, {
    selectedStatus <- as.numeric(input$selectStatus)
    if(selectedStatus > 0){
      selectedEdgesId <- edges()[edges()$status_id == selectedStatus, 'id']
      # browser()
      visSelectEdges(myVisNetworkProxy, selectedEdgesId)
    }
    else
      visUnselectAll(myVisNetworkProxy)
  })
  
  # For Download data
  fdf <- reactive({
    # fe = filtered_edges
    tmp_df <- df[df$map_name==input$dd_map_name | 
                input$dd_map_name=='All', c('from', 'to', 'group1_name', 
                                            'group2_name', 'year', 'status',
                                            'description', 'map_name', 'primary',
                                            'title')]
    tmp_df[tmp_df$year >= input$dd_range[1] & tmp_df$year <= input$dd_range[2],]
    
  })
  
  output$dataTable <- renderDataTable({
    fdf()[, !(colnames(fdf()) %in% c('description'))]
  }, 
    options = list(pageLength=5)
  )
  
    observeEvent('downloadData',{
    output$downloadData <- downloadHandler(
      filename <- function(){"data.csv"},
      content <- function(file){
        write.csv(fdf(), file)
      }
    )
  })
    

    
    #------------------Sankey graph----------------------------------
    
    filtered_df_sankey <-reactive({
      df %>%
        dplyr::filter(input$s_map_name  == 'All' | map_name == input$s_map_name) %>%
        dplyr::filter(year >= input$range[1] & year <= input$range[2] ) %>%
        dplyr::filter(status=="Splinters" | status == "Mergers")
    })
    
    
    nodes_sankey <- reactive({
      data.frame(
        name = unique(c(as.character(filtered_df_sankey()$group1_name),
                        as.character(filtered_df_sankey()$group2_name))))
    })
    
    links_sankey <- reactive({ 
      data.frame(map_name= filtered_df_sankey()$map_name,
                 source = filtered_df_sankey()$group1_name,
                 target = filtered_df_sankey()$group2_name,
                 value  = rep(1, length(filtered_df_sankey()$group1_name)),
                 IDsource = match(filtered_df_sankey()$group1_name, nodes_sankey()$name) - 1,
                 IDtarget = match(filtered_df_sankey()$group2_name, nodes_sankey()$name) - 1
      )
    })
    
    output$diagram <- renderSankeyNetwork({
      # browser()
      sankeyNetwork(
        Links = links_sankey(),
        Nodes = nodes_sankey(),
        Source = "IDsource",
        Target = "IDtarget",
        Value = "value",
        NodeID = "name",
        #units = 'TWh', 
        #iterations=100,
        #fontSize = 12, 
        nodeWidth = 30,
        sinksRight = FALSE
      )
    })
    
    output$nvf_legend_sub <- renderUI({
      
      # browser()
      svg_content <- get_legend(ledges)
      HTML(
        paste0("
        <style>
        .legend_label
        {
          font-size:15pt;
        }
        </style>
        </br>
        <h2 style='width:100px;'>Legend</h2>
        <svg viewBox='0 0 373 500' xmlns='http://www.w3.org/2000/svg'>",
        paste0(
           svg_content, "</svg>"))
      )
    })
    
    output$reg_hideDesc <- renderUI({
      HTML(
        "<script>
        document.getElementById('mp').addEventListener('click', hideDesc);
        </script>"
      )
    })
    #---------------------Geography map code----------------------------------
    
    remove_loop2 <- function(nodes, edges)
    {
      hqcs <- nodes$hqc
      rejection_list <- c()
      for(i in 1:length(hqcs))
      {
        hq <- hqcs[i]
        indices <- which(edges$g1_hqc==hq & edges$g2_hqc==hq)
        # We do not want these edges
        if(length(indices) > 0)
        {
          rejection_list <- c(rejection_list, indices)
        }
      }
      edges <- edges[-rejection_list,]
    }
    
    # Purpose of this part of the code is to provide unique edges between
    # two headquarter countries that handles both NA, empty strings, loop
    # connection and bidirectional edges.
    geo.edges <- reactive({
      
      # Embed location information to both first and the second groups in the 
      # edges dataframe.
      tmp.edges <- unique(df[, c('from', 'to')] )
      tmp.edges <- tmp.edges %>% inner_join(unique(df_nodes[, c('id', 'hq_country')]), 
                                            by=c('from'='id'), copy=T)
      cnames <- colnames(tmp.edges)
      cnames[cnames=='hq_country'] <- 'g1_hqc'
      colnames(tmp.edges) <- cnames
      tmp.edges <- drop_na(tmp.edges)
      tmp.edges <- tmp.edges[str_trim(tmp.edges$g1_hqc, side='both') != "",]
      
      tmp.edges <- tmp.edges %>% inner_join(unique(df_nodes[, c('id', 'hq_country')]), 
                                            by=c('to'='id'), copy=T)
      cnames <- colnames(tmp.edges)
      cnames[cnames=='hq_country'] <- 'g2_hqc'
      colnames(tmp.edges) <- cnames
      tmp.edges <- drop_na(tmp.edges)
      tmp.edges <- tmp.edges[str_trim(tmp.edges$g2_hqc, side='both') != "",]
      nonunique.edges <- tmp.edges
      tmp.edges <- unique(tmp.edges[, c('g1_hqc', 'g2_hqc')])
      
      # Assign ID for HQC as if you would for nodes
      tmp.nodes <- unique(c(tmp.edges$g1_hqc, tmp.edges$g2_hqc))
      tmp.nodes <- data.frame(id=seq(1,length(tmp.nodes)), hqc=tmp.nodes)
      
      # Join for creating from and to in tmp.edges
      tmp.edges <- tmp.edges %>% inner_join(tmp.nodes, by=c("g1_hqc"="hqc"))
      cnames <- colnames(tmp.edges)
      cnames[cnames=='id'] <- 'from'
      colnames(tmp.edges) <- cnames
      
      tmp.edges <- tmp.edges %>% inner_join(tmp.nodes, by=c("g2_hqc"="hqc"))
      cnames <- colnames(tmp.edges)
      cnames[cnames=='id'] <- 'to'
      colnames(tmp.edges) <- cnames
      
      tmp.edges$id <- seq(1, nrow(tmp.edges))
      # browser()
      tmp.edges <- remove_loop2(tmp.nodes, tmp.edges)
      tmp.edges <- remove_bidirection(tmp.edges)
      
      # Embed coordinate information
      tmp.edges <- tmp.edges %>% inner_join(coords, by=c('g1_hqc'='hq_country'))
      cnames <- colnames(tmp.edges)
      cnames[cnames=='latitude'] <- 'latitude1'
      cnames[cnames=='longitude'] <- 'longitude1'
      colnames(tmp.edges) <- cnames
      
      
      tmp.edges <- tmp.edges %>% inner_join(coords, by=c('g2_hqc'='hq_country'))
      cnames <- colnames(tmp.edges)
      cnames[cnames=='latitude'] <- 'latitude2' 
      cnames[cnames=='longitude'] <- 'longitude2'
      colnames(tmp.edges) <- cnames
      
      return(list(tmp.edges, nonunique.edges)) # tmp.edges is unique edges
    })
    
    
    nodes_geo <- reactive({
      unique.edges <- geo.edges()[[1]]
      tmp.df <- geo.edges()[[2]]
      
      unique_hq <- unique(c(unique.edges$g1_hqc, unique.edges$g2_hqc))
      
      mp_coords <- data.frame(hq_country=unique_hq)
      mp_coords$lat <- 0
      mp_coords$long <- 0
      mp_betweenness <- 0
      
      for(i in 1:length(unique_hq))
      {
        hq <- unique_hq[i]
        tmp.edges <- tmp.df[tmp.df$g2_hqc==hq, c('from', 'to')]
        tmp.nodes <- unique(c(tmp.edges$from, tmp.edges$to))
        tmp.nodes <- data.frame(id=tmp.nodes) %>% 
                        inner_join(nodes[, c('id', 'value')], by='id')
        avg_degree <- mean(tmp.nodes$value)
        
        coord <- coords[coords$hq_country==hq,]
        
        mp_coords[mp_coords$hq_country==hq, c('lat', 'long')] <- 
                              coord[, c('latitude', 'longitude')]
        
        # Use betweenness centrality to color the nodes
        # browser()
        mp_coords[mp_coords$hq_country==hq, 'degree'] <- avg_degree
        # mp_coords[mp_coords$map_name==mn, 'count'] <- n_edges
      }
      # browser()
      tmp.colorPalette <- colorRampPalette(c('blue', 'red'))(7)
      counts_cut <- cut(mp_coords$degree, 7)
      node.color <- tmp.colorPalette[as.numeric(counts_cut)]
      mp_coords$color <- node.color
      
      mp_coords
    })
    
    output$geoMap <- renderPlot({
      
      lat_range <- range(nodes_geo()$lat)
      lon_range <- range(nodes_geo()$long)
      
      maps::map(database="world", 
                border="gray10", fill=T, bg='black', col="grey20")
      # browser()
      points(x=nodes_geo()$long, y=nodes_geo()$lat, pch=19, 
             col=nodes_geo()$color, cex=2)
      text(x=nodes_geo()$long, y=nodes_geo()$lat-5, label=nodes_geo()$hq_country, 
           cex=1.24, col='white')
      
      unique.edges <- geo.edges()[[1]]
      tmp.edges <- geo.edges()[[2]] # non unique edges so we can get the count of
      # connections between two hq countries
      
      # Color the edges---------------------------------------------------------
      
      unique.edges$count <- 0
      for(i in 1:nrow(unique.edges))
      {
        # browser()
        m1_m2 <- unique.edges[i,]
        count <- nrow(tmp.edges[tmp.edges$g1_hqc==m1_m2$g1_hqc &
                                tmp.edges$g2_hqc==m1_m2$g2_hqc,])
        
        unique.edges[unique.edges$g1_hqc==m1_m2$g1_hqc & 
                     unique.edges$g2_hqc==m1_m2$g2_hqc, 'count'] <- count
      }
      tmp.colorPalette <- colorRampPalette(c('orange', 'red'))(7)
      count_binned <- cut(unique.edges$count, 7)
      unique.edges$color <- tmp.colorPalette[as.numeric(count_binned)]
      
      for(mn_idx in 1:nrow(unique.edges))
      {
        coord1 <- unique.edges[mn_idx, c('longitude1', 'latitude1')]
        coord2 <- unique.edges[mn_idx, c('longitude2', 'latitude2')]
        color <- unique.edges[mn_idx, 'color']
        # browser()
        intEdges <- gcIntermediate(coord1, coord2, n=1000, addStartEnd=T)
        lines(intEdges, col=color, lwd=2)
      }
      
      # longitudes <- c()
      # latitudes <- c()
      # for(i in 1:nrow(unique.edges))
      # {
      #   lat <- c(unique.edges[i, 'latitude1'], unique.edges[i, 'latitude2'])
      #   lon <- c(unique.edges[i, 'longitude1'],unique.edges[i, 'longitude2'])
      #   lines(x=lon, y=lat, col=unique.edges[i, 'color'], lwd=2)
      # }
      # browser()
      
    })
    
    #---------------------tm_map plot-------------------------------------------
    
    available.provinces <- reactive({
      tmp.df <- unique(df_nodes[df_nodes[, input$geo_filter_col] > 0 &
                                (!is.na(df_nodes$hq_province)) &
                                (length(df_nodes$hq_province) > 0) &
                                (!is.na(df_nodes$hq_country)) & 
                                length(df_nodes$hq_country) > 0, 'hq_province'])
    })
    
    # observeEvent(input$geo_filter_col, {
    #   updateSelectInput(session, 'geo_province', choices=available.provinces(),
    #                     selected=available.provinces())
    # })
    
    tmplot.map <- reactive({
      
      debug_param <- available.provinces()
      # browser()
      
      # The following line need to be thrown onto global.R file
      province_country <- unique(drop_na(df_nodes[, c('hq_province', 'hq_country')]))
      
      # Map invalid country names to valid ones
      # browser()
      province_country <- province_country %>% 
                           inner_join(valid.countries.list, 
                                           by=c('hq_country'='hq_country_old'))
      cnames <- colnames(province_country)
      cnames[cnames=='hq_country'] <- 'hq_country_invalid'
      cnames[cnames=='hq_country_new'] <- 'hq_country'
      colnames(province_country) <- cnames
      # browser()
      
      # province <- input$geo_province
      
      # cnames <- colnames(province.info)
      # cnames[cnames=='hq_province'] <- 'name'
      # colnames(province.info) <- cnames
      world <- ne_countries(scale='medium', returnclass='sf')
      world <- sf::st_as_sf(world)
      world <- sf::st_make_valid(world)
      geo_df <- NULL
      complete_df <- NULL
      
      if(input$geo_province=='All')
      {
        # World shapefile
        
        geo_df <- ne_countries(scale='medium', returnclass = 'sf')
        for(province in available.provinces())
        {
          # Get the corresponding country name
          country <- province_country[province_country$hq_province==province,
                                      'hq_country']
          country <- unique(country[!is.na(country)])
          # browser()
          
          # download shapefile from rnaturalearthhire
          geo_df <- ne_states(country=country, returnclass='sf')
          # browser()
          
          if(sum(str_detect(geo_df$name, province)) > 0)
          {
            # Fetch all the nodes that belong to the province
            province.nodes <- unique(df_nodes[df_nodes$hq_province==province, 
                                              c('id', 'label', 'init_size_members', 
                                                'max_size_members', 'hq_province')])
            
            # Filter out all nodes with NA in ID
            province.nodes <- province.nodes[!is.na(province.nodes$id),]
            
            # Summary information about a province
            province.info <- province.nodes |>
              group_by(hq_province) |>
              summarise(init_size_members=sum(init_size_members, na.rm=T),
                        max_size_members=sum(max_size_members, na.rm=T),
                        n_profiles=n())
            
            # Join profile summary with shape information in geo_df dataframe
            tmp.info <- geo_df |> 
              regex_inner_join(province.info, by=c('name'='hq_province'))
            tmp.info <- sf::st_as_sf(tmp.info)
            tmp.info <- sf::st_make_valid(tmp.info)
            
            complete_df <- tm_shape(world, xlim=c(-150, 180), ylim=c(-60,90)) + 
                            tm_polygons()
 
            # browser()
            complete_df <- complete_df + 
                            tm_shape(tmp.info) + 
                            tm_polygons(input$geo_filter_col)

          }
        }
      }
      else
      {
        # # Country shapefile
        # browser()
        # geo_df <- ne_states(country=country, returnclass='sf')
        # # province.info <- province.info |> 
        # #                  inner_join(geo_df, by=c('hq_province'='name'))
        # province.info <- geo_df |> 
        #                  regex_inner_join(province.info, by=c('name'='hq_province'))
        # province.info <- st_as_sf(province.info)
        # # browser()
      }
      # browser()
      return(complete_df)
    })
    
    output$tmplot <- renderPlot({
      tmplot.map()
    })
    
    
    #-------------------- Hierarchical code------------------------------------
    
    dfs <- reactive({
      
      h_edges <- df[df$map_name==input$h_map_name & (df$status=='Splinters' | 
                                                     df$status=='Mergers'), ]
      # browser()
      nodes_mn <- unique(c(h_edges$from, h_edges$to))
      nodes_mn <- data.frame(id=nodes_mn) %>% 
                        inner_join(nodes, by='id', copy=T)
      cnames <- colnames(nodes_mn)
      cnames[cnames=='level'] <- 'year'
      colnames(nodes_mn) <- cnames
      
      dfs <- preprocess_hdata(h_edges, nodes_mn)
      nodes_mn <- dfs[[1]]
      h_edges <- dfs[[2]]
      
      dfs <- get_all_done(nodes_mn, h_edges)
      nodes_mn <- dfs[[1]]
      nodes_mn<- nodes_mn %>% select(-width)
      # Inspect and understand where value is given to edges and what does this
      # attribute mean
      h_edges <- h_edges %>% select(-value, -label)
      return(list(nodes_mn, h_edges))
    })
    
    output$visnetworktimeline <- renderVisNetwork({
      tmp.edges <- dfs()[[2]]
      cnames <- colnames(tmp.edges)
      width_idx <- which(cnames=='width')
      tmp.edges <- tmp.edges[, -width_idx]

      
      visNetwork(dfs()[[1]], tmp.edges) %>%
        visEdges(
          arrows=list(to=list(enabled=T))) %>%
        visPhysics(enabled = F) %>% 
        visEvents(type='once',
                  beforeDrawing=sprintf("function(){
                                          this.moveTo({scale:1,
                                                       position: {x:1150, y:450},
                                                       });
                                          
                                         }")#,
                  # afterDrawing="function(){
                  #                           let pos = this.getViewPosition();
                  #                           let posY = pos.y;
                  #                           alert(posY);
                  #                       }"
                  ) %>%
        visEvents(zoom = "function(properties){
                            Shiny.setInputValue('zoomDel', properties);
                          }",
                  dragEnd = "function(properties)
                              {
                                Shiny.setInputValue('dragDel', properties);
                              }"
                  ) %>%
        visInteraction(zoomView = F) %>%
        visOptions(autoResize=F)
    })
    
    h_networkProxy <- visNetworkProxy("visnetworktimeline")
    
    output$year_ruler_sub <- renderUI({
      
      #--------------------Timeline ruler---------------------------------------
      tmp.nodes <- dfs()[[1]]
      
      tmp.levels <- unique(tmp.nodes[, c('year', 'y')])
      tmp.levels <- tmp.levels %>% arrange(year)
      svg_content <- get_h_legend(tmp.levels)
      HTML(
        paste0("
        <style>
          .legend_label
          {
            font-size:15pt;
          }
        </style>
        <!--<h2 style='width:100px;'>Legend</h2>-->
        </br>
        <svg viewBox='0 0 130 3000' xmlns='http://www.w3.org/2000/svg'>",
               paste0(
                 svg_content, "</svg>"))
      )
    })
    
    output$h_legend_sub <- renderUI({
      # browser()
      ledges <- dfs()[[2]]
      ledges <- unique(ledges[, c('status', 'color')])
      cnames <- colnames(ledges)
      cnames[cnames=='status'] <- 'label'
      colnames(ledges) <- cnames
      svg_content1 <- get_legend(ledges, include_second_line=F)
      svg_content2 <- "
            <circle cx='20' cy='200' r='20' fill='#97C2FC' />
            <text x='60px' y='200' class='legend_label'>Original Node</text>
            <circle cx='20' cy='260' r='20' fill='#FB7E81' />
            <text x='60px' y='260' class='legend_label'>Clone Node</text>
      "
      svg_content <- paste(svg_content1, svg_content2)
      HTML(
        paste0("
        <style>
          .legend_label
          {
            font-size:15pt;
          }
        </style>
        <h2 style='width:100px;'>Legend</h2>
        </br>
        <svg viewBox='0 0 200 1000' xmlns='http://www.w3.org/2000/svg'>",
               paste0(
                 svg_content, "</svg>"))
      )
    })
    
    observeEvent(input$zoomDel, {
      # browser()
      direction <- input$zoomDel[['direction']]
      
      session$sendCustomMessage('scaleLegend', direction)
    })
    
    observeEvent(input$dragDel, {
      # browser()
      # logging::loginfo(input$dragDel)
      deltaY <- (input$dragDel[['event']])$deltaY
      # angle <- ((input$dragDel[['event']])$center)$y
      session$sendCustomMessage('moveLegend', deltaY)
    })
    
    #-------------------------Statistical Plot--------------------------------------
    
    stat_rel_df <- reactive({
      map_name <- input$s_map_name
      tmp.df <- df[df$map_name==map_name, c('group1_name', 'group2_name', 'year', 
                                            'label')]
    })
      
      growth_by_prof <- reactive({
        tmp.df <- df_nodes %>% inner_join(nodes[, c('id', 'between')], by='id')
        tmp.df <- unique(tmp.df[, c('label', 'init_size_members', 
                                    'max_size_members', 'between', 
                                    'level', 'endyear')])
        tmp.df <- tmp.df[(!is.na(tmp.df$init_size_members)) & 
                           (!is.na(tmp.df$max_size_members)),]
        tmp.df <- tmp.df[tmp.df$init_size_members!="" & 
                           tmp.df$max_size_members!="",]
        
        tmp.df$new.endyear <- ifelse(tmp.df$endyear==0, 
                                     as.integer(format(Sys.Date(), "%Y")),
                                     tmp.df$endyear)
        tmp.df$period <- tmp.df$new.endyear - tmp.df$level
        tmp.df$init_size_members <- clean_size_members(tmp.df$init_size_members)
        tmp.df$init_size_members <- as.integer(tmp.df$init_size_members)
        tmp.df$max_size_members <- clean_size_members(tmp.df$max_size_members)
        tmp.df$max_size_members <- as.integer(tmp.df$max_size_members)
        
        growth_by_profile <- tmp.df %>%
          group_by(label) %>% 
          summarise(init_size=mean(init_size_members),
                    max_size=mean(max_size_members),
                    between=mean(between),
                    period=mean(period))
        attach(growth_by_profile)
        growth_by_profile$growth <- (max_size-init_size)/period
        detach(growth_by_profile)
        growth_by_profile <- growth_by_profile %>% arrange(desc(growth))
        growth_by_profile <- as.data.frame(growth_by_profile)
        growth_by_profile[1:input$stats_sample_size,]
      })
      
      output$membersGrowth <- renderPlot({
        
        ggplot(growth_by_prof(), aes(x=reorder(label, growth), 
                                             y=growth, fill=between)) +
        geom_bar(stat='identity') +
        xlab('Profile Name') + ylab('Growth per year') + 
          ggtitle('Growth in member size on average by Profile') +
        coord_flip() + 
        guides(fill=guide_legend(title="Profile's influence")) +
          theme_minimal()
      })
      
      activeProfiles <- reactive({
        sample.size <- input$stats_sample_size # temporarily disconnecting this
        tmp.df <- admin.profiles()
        tmp.df <- unique(tmp.df[, c('label', 'level', 'activeC')]) %>%
                    filter(activeC == 'Active' & level != 0) %>%
                    arrange(level)
        cnames <- colnames(tmp.df)
        cnames[cnames %in% c('level', 'activeC')] <- c('Start Year', 'Active')
        colnames(tmp.df) <- cnames
        tmp.df
      })
      
      output$showActiveProfiles <- renderDataTable(activeProfiles(),
                                                   options=list(pageLength=10,
                                                                scrollY='430px')
                                                  )
      
      output$basicStats_map <- renderPlot({
        tmp.df <- df[, c('group1_name', 'group2_name', 'year', 'label', 'map_name')]
        ggplot(tmp.df, aes(x=year, fill=label)) + 
          geom_bar(position='dodge', stat='count') +
          theme(axis.text = element_text(size = 20),
                plot.title = element_text(size=30),
                axis.title.x = element_text(size=20),
                axis.title.y = element_text(size=20),
                legend.text = element_text(size=20),
                legend.title = element_text(size=20),
                legend.position = 'top') +
          facet_wrap(~map_name, scales='free', nrow=7)
      })
      
      output$activeg_year <- renderPlot({
        ag_df <- NULL
        for(i in 1:length(maps))
        {
          map_name <- maps[i]
          tmp.ag.df <- get_activeg_df(df_nodes, map_name)
          tmp.ag.df$map_name <- map_name
          if(is.null(tmp.ag.df))
          {
            ag_df <- tmp.ag.df
          }
          else
            ag_df <- rbind(ag_df, tmp.ag.df)
        }
        
        ag_df <- data.frame(ag_df) %>% filter(year != 0)
        
        ggplot(ag_df, aes(x=year, y=n_active_groups, color=n_active_groups)) +
          geom_point(color='steelblue') +
          geom_line() +
          xlab('Year') + ylab('Number of active groups') +
          ggtitle('Number of active groups each year') +
          # theme(panel.background = element_rect(fill='transparent')) +
          facet_wrap(~map_name, nrow = 6, scales='free')
      })
      
      output$nprofiles_map <- renderPlot({
        tmp.df_nodes <- unique(df_nodes[, c('label', 'map_name')])
        tmp.df_nodes <- tmp.df_nodes %>% group_by(map_name) %>%
                                         summarise(count=n()) %>%
                                         arrange(desc(count))
        avg_n <- mean(tmp.df_nodes$count)
        
        ggplot(tmp.df_nodes, aes(x=reorder(map_name, -count), y=count, fill=count)) +
          geom_bar(stat='identity', ) +
          geom_hline(yintercept=avg_n, color='red') +
            xlab('Map Name') +
            ylab('Number of unique profiles') +
            ggtitle('Number of unique profiles in each map') + 
            theme(axis.text = element_text(size = 20, angle=90),
                  plot.title = element_text(size=30),
                  axis.title.x = element_text(size=20),
                  axis.title.y = element_text(size=20))
        
        # ggplot(tmp.df_nodes, aes(x=map_name, y=count, color=count)) +
        # geom_point(color='steelblue') +
        #   geom_line() +
        #   xlab('Map Name') +
        #   ylab('Number of unique profiles') +
        #   ggtitle('Number of unique profiles in each map')
        
      })
      
      output$top.profiles.most.edges <- renderPlot({
        
        top_profiles <- NULL
        for(i in 2:length(maps))
        {
          mname <- maps[i]
          unique.nodes <- unique(df_nodes[df_nodes$map_name==mname, 'label'])
          tmp.profiles <- NULL
          for(j in 1:length(unique.nodes))
          {
            node_label <- unique.nodes[j]
            n <- nrow(unique(df[df$group1_name == node_label |
                         df$group2_name == node_label,]))
            # browser()
            if(is.null(tmp.profiles))
              tmp.profiles <- data.frame(map_name=mname, label=node_label, n=n)
            else
              tmp.profiles <- rbind(top_profiles, 
                                    data.frame(map_name=mname, label=node_label, n=n))
          }
          tmp.profiles <- tmp.profiles %>% arrange(desc(n))
          tmp.profiles <- tmp.profiles[1:5,]
          
          if(is.null(top_profiles))
            top_profiles <- tmp.profiles
          else
            top_profiles <- rbind(top_profiles, tmp.profiles)
        }
        # browser()
        ggplot(top_profiles, aes(x=label, y=n)) +
        geom_bar(stat='identity') +
          xlab('Profile Name') +
          ylab('Number of edges') +
          ggtitle('Number of unique profiles in each map') + 
          # theme(axis.text = element_text(size = 20, angle=90),
          #       plot.title = element_text(size=30),
          #       axis.title.x = element_text(size=20),
          #       axis.title.y = element_text(size=20)) +
          facet_wrap(~map_name, scales='free')
        
      })
      
      # For t-test
      # browser()
      # n_edges.map_name <- df %>% group_by(primary) %>% summarise(count=n())
      output$showTtest <- renderText({
        
      })
    
    #------------------------Administration------------------------------------
    
    # Edit Maps
    admin.maps <- reactive({
      data.frame(name=unique(df_nodes$map_name),
                 edit_links=sprintf('<a href=\"javascript:Shiny.setInputValue(\'editMap\', \'%s\');\"><i class=\"fa fa-link\"></i></a>', 
                                  unique(df_nodes$map_name)),
                 manage=sprintf('<a href=\"javascript:Shiny.setInputValue(\'manageMap\', \'%s~@~\'+Date.now());\">Manage</i></a>',
                                unique(df_nodes$map_name))
                 )
    })
    
    output$em_profiles <- renderUI({
      # browser()
      html.table <- "<table class='admin_table'><tr class='tr_class'><th class='th_class'>Map Name</th><th class='th_class'>Edit Links</th><th class='th_class'>Manage</th></tr>"
      html.inner <- ""
        for(i in 1:nrow(admin.maps()))
        {
          tmp.label <- admin.maps()[i, 'name']
          tmp.url <- admin.maps()[i, 'edit_links']
          tmp.manage <- admin.maps()[i, 'manage']
          
          html.inner <- paste0(html.inner, sprintf("<tr class='tr_class'><td class='td_class'>%s</td>
                                                        <td class='td_class'>%s</td>
                                                        <td class='td_class'>%s</td></tr>",
                                                   tmp.label, tmp.url, tmp.manage
                                                   ))
        }
      html.table <- paste0(html.table, paste0(html.inner, '</table>'))
      html.table <- HTML(html.table)
      html.table
    })
    
    observeEvent(input$newProf_btn, {
      updateTextInput(session, inputId='new_prof_mn', value='')
      shinyjs::disable("new_prof_mn")
      session$sendCustomMessage('toggleNewProf_div', input$newProf_btn)
    })
    
    observeEvent(input$new_prof_map, {
      if(input$new_prof_map == "Other")
      {
        shinyjs::enable('new_mn')
      }
      else
      {
        shinyjs::disable('new_mn')
      }
    })
    
    observeEvent(input$newProf_schanges, {
      # browser()
      warnings <- c()
      end_year <- as.integer(input$newProf_schanges['ey'][[1]])
      start_year <- as.integer(input$newProf_schanges['sy'][[1]])
      init_size_members <- as.integer(input$newProf_schanges['ism'][[1]])
      max_size_members <- as.integer(input$newProf_schanges['msm'][[1]])
      first_attack <- as.integer(input$newProf_schanges['first_attack'][[1]])
      name <- input$newProf_schanges['name'][[1]]
      url <- input$newProf_schanges['url'][[1]]
      desc <- input$newProf_schanges['desc'][[1]]
      active <- input$newProf_schanges['active'][[1]]
      complete <- input$newProf_schanges['complete'][[1]]
      map_name <- input$new_prof_map
      city <- input$newProf_schanges['city']
      province <- input$newProf_schanges['province']
      country <- input$newProf_schanges['country']
      
      if(map_name == "Other")
      {
        map_name <- input$new_mn
        if(length(map_name) < 3)
          warnings <- c(warnings, 'New map name cannot be empty')
      }
      
      if(length(city) < 3)
      {
        warnings <- c(warnings, 'City field cannot be empty')
      }
      
      if(length(province) < 3)
      {
        warnings <- c(warnings, 'Province cannot be empty')
      }
      
      if(length(country) < 3)
      {
        warnings <- c(warnings, 'Country cannot be empty')
      }
      
      browser()
      
      if(length(name) < 3)
      {
        warnings <- c(warnings, 'Profile name cannot be empty')
      }
      
      if(!is.na(end_year))
      {
        if(end_year > as.integer(str_extract(Sys.Date(), '\\d{4}')) |
           end_year < start_year)
        {
          warnings <- c('Invalid end year!')
        }
      }
      
      if(!is.na(init_size_members) & !is.na(max_size_members))
      {
        if(init_size_members > max_size_members)
        {
          warnings <- c(warnings, 'Invalid max size members')
        }
      }
      else
        warnings <- c(warnings, 'Both Initial and later members size must
                      be supplied')
      
      if(!is.na(first_attack))
      {
        if(first_attack < start_year)
        {
          warnings <- c(warnings, 'Invalid first attack year')
        }
      }
      
      # browser()
      if(length(warnings) > 0)
      {
        session$sendCustomMessage('showWarnings', '')
        output$new_prof_warnings <- renderText({warnings[1]})
      }
      else
      {
        output$new_prof_warnings <- renderText({})
        session$sendCustomMessage('hideWarnings', '')
        
        # Save the changes
        
      }
      
    })
    
    # Open manage a map div with information about the map populated
    observeEvent(input$manageMap, {
      mm_map_name <<- str_split(input$manageMap, '~@~')[[1]][1]
      map_info <- unique(df_nodes[df_nodes$map_name==mm_map_name, 
                                                               c('map_name',
                                                                 'new_description',
                                                                 'URL',
                                                                 'level',
                                                                 'endyear')])[1,]

      session$sendCustomMessage('showEditMap', map_info)
    })
    
    observeEvent(input$showIncludedGroups, {
      map_name <- input$showIncludedGroups
      map_name <- str_split(map_name, '~@~')[[1]][1]
      logging::loginfo(map_name)
      nodes_under_mn <- data.frame(Profile=df_nodes[df_nodes$map_name==map_name, c('label')])
      output$includedGroupsTable <- renderDataTable(
        nodes_under_mn
      )
    })
    
    # Close manage a map div
    observeEvent(input$em_mp_back, {
      session$sendCustomMessage('closeEditMap', '')
    })
    
    observeEvent(input$em_mp_save, {
      # session$sendCustomMessage('saveMapChanges', mm_map_name)
      #*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      # Make sure to update this when Zoom levels and Included groups are implemented------------------
      #*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

    })
    
    admin.profiles <- reactive({
      tmp.profiles <- df_nodes.copy
      tmp.profiles$activeC <- ifelse(tmp.profiles$active==1, "Active", 
                                     tmp.profiles$endyear)
      tmp.profiles
      # browser()
    })

    output$ep_profiles <- renderPrint({

      profiles <- admin.profiles()
      html.table <- "<table class='admin_table' id='edit_profiles'>
                      <tr class='tr_class'>
                        <th class='th_class'>Name</th>
                        <th class='th_class'>Founded</th>
                        <th class='th_class'>Dislanded</th>
                        <th class='th_class'>Hide</th>
                        <th class='th_class'>View</th>
                        <th class='th_class'>Backup</th>
                        <th class='th_class'>Delete</th>
                      </tr>"

      html.inner <- ""
      for(i in 1:nrow(profiles))
      {
        profile <- profiles[i,]
        name <- profile$label
        level <- profile$level
        active <- profile$activeC
        # browser()
        hidden <- ifelse(name %in% h.profile_names, T, F)
        hidden.html <- ""
        if(hidden==T)
        {
          hidden.html <- sprintf("<td class='td_class'><input type='checkbox' onload=\"javascript:console.log('Hello...');\" onchange=\"javascript:Shiny.setInputValue('hide_profile', '%s');\" checked></td>", name) 
        }
        else
        {
          hidden.html <- sprintf("<td class='td_class'><input type='checkbox' onload=\"javascript:console.log('Hello...');\" onchange=\"javascript:Shiny.setInputValue('hide_profile', '%s');\"></td>", name) 
        }
        
        html.inner <- paste0(html.inner, sprintf("<tr class='tr_class'>
                                                    <td class='td_class'>%s</td>
                                                    <td class='td_class'>%d</td>
                                                    <td class='td_class'>%s</td>
                                                    %s
                                                    <td class='td_class'><a href=\"javascript:Shiny.setInputValue('view_profile', '%s');\"><i class=\"fa fa-eye\"></i></a></td>
                                                    <td class='td_class'><a href=\"javascript:Shiny.setInputValue('backup_profile', '%s');\">Backup</a></td>
                                                    <td class='td_class'><button type='button' onclick=\"javascript:Shiny.setInputValue('delete_profile', '%s');\">Delete</button></td>
                                                 </tr>", name, level, active, hidden.html,
                                                 name, name, name, name, quote=F))
      }
      html.inner <- paste0(html.table, paste0(html.inner, "</table>"))
      cat(html.inner)
    })
    
    observeEvent(input$hide_profile, {
      # I just assigned profile_name for brevity...
      profile_name <- input$hide_profile
      if(profile_name %in% h.profile_names)
      {
        index <- which(h.profile_names==profile_name)
        h.profile_names <<- h.profile_names[-index]
      }
      else
        h.profile_names <<- c(h.profile_names, profile_name)
      
      ep_changes_made <<- T
    })
    
    
    observeEvent(input$delete_profile, {
      
      # Get the original index in the df_nodes corresponding to the deleted profile
      # in order to remove the row from the table
      profile_name <- input$delete_profile
      
      d.profile_names <<- c(d.profile_names, profile_name)
      
      # df_nodes.original is the sorted version of df_nodes
      # so it picks the correct row.id
      # The following snippet is for removing the record from the HTML table
      row.id <- which(df_nodes.copy$label %in% profile_name)
      session$sendCustomMessage('removeRow', row.id)
      df_nodes.copy <<- df_nodes.copy[-row.id,]
      ep_changes_made <<- T
    })
    
    observeEvent(input$ep_save_changes, {
      
      # Here before you apply the changes, get the anti-join between the
      # df_nodes and df_nodes.copy so that you could get the ids of the profiles
      # that you had deleted. This would enable you to retrieve the remaining
      # columns instead of saving the file with just 4 or 5 selective columns
      # and losing invaluable information.
      # browser()
      if(ep_changes_made==T)
      {
        # Delete profiles section-----------------------------------------------
 
        # Restoring column names so that when we reload the file
        # there wouldn't be any conflicts
        
        #-------Relationships dataframe
        l_rel_fname <- get_latest_file('data/relationships', 'relationships')
        tmp.df <- read.csv(paste0('data/relationships/',l_rel_fname), sep=',', 
                           header=T, fileEncoding = 'UTF-8-BOM', check.names=T,
                           colClasses=c('multiple'='factor'))
        if(length(d.profile_names) > 0)
        {
          indices <- which(tmp.df$group1_name %in% d.profile_names | 
                             tmp.df$group2_name %in% d.profile_names)
          tmp.df <- tmp.df[-indices,] 
        }
        
        #--------Nodes dataframe
        latest_fname <- get_latest_file('data/groups/', 'groups')
        tmp.df_nodes <- read.csv(paste0('data/groups/',latest_fname), header=T,)
        if(length(d.profile_names) > 0)
        {
          indices <- which(tmp.df_nodes$group_name %in% d.profile_names)
          tmp.df_nodes <- tmp.df_nodes[-indices,]
        }
        
        # Save hidden profile names changes--------------------------------------
        save(file='data/hidden_profiles.RData', 'h.profile_names')
        #-----------------------------------------------------------------------
        
        time_str <- str_extract(Sys.time(), '\\d{2}:\\d{2}:\\d{2}')
        time_str <- gsub(":", "_", time_str)
        date_str <- gsub("-", "_", Sys.Date())
        date_time <- paste0(date_str, paste0('-', time_str))
        
        g.fname <- paste0(paste0("groups", date_time), ".csv")
        r.fname <- paste0(paste0("relationships", date_time), ".csv")
        write.csv(tmp.df_nodes, file=paste0('data/groups/', g.fname))
        write.csv(tmp.df, file=paste0('data/relationships/', r.fname))
        
        # Once changes are saved, then we can reset these parameters
        ep_changes_made <<- F
        d.profile_names <<- NULL
        session$sendCustomMessage('refresh_page', '')
        
        # Apply the changes to the local variables for continuous use...
        # Delete profiles from df_nodes
        d.profile_names
        indices <- which(df_nodes$label %in% d.profile_names)
        df_nodes <<- df_nodes[-indices,]
        
        # Delete edges related to the discarded profile
        indices <- which(df$group1_name %in% d.profile_names |
                           df$group2_name %in% d.profile_names)
        df <<- df[-indices,]
        
        
        # Re-assign....
        df_nodes.copy <<- unique(df_nodes[, c('id', 'label', 'level', 'active', 
                                                   'URL', 'endyear')])
        df_nodes.copy.original <<- df_nodes.copy
        df.copy <<- df
        
      }
    })
    
    observeEvent(input$ep_discard_changes, {
      d.profile_names <<- NULL
      ep_changes_made <<- F
      
      # Append more code when more flexibility to customization is added
    })
    
    observeEvent(input$view_profile, {
      
    })
    
    
    output$mapOutput <- renderLeaflet({
      browser()
      leaflet(nodes_geo()) %>%
        addProviderTiles(providers$Esri.WorldTopoMap) %>%
        addMarkers(~long, ~lat, 
                   # icon = filteredIcon(), 
                   # label = ~Player, 
                   labelOptions = labelOptions(textsize = "12px"),
                   popup = ~label
                   )
    })
    
})






















