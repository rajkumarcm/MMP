
# Note------------------------------------------------------------------------
# In the Sankey graph, connections are filtered to be either Splinters or 
# Mergers.
# dplyr::filter(status=="Splinters" | status == "Mergers")
# Why did they do this ?
#-----------------------------------------------------------------------------

library('maps')
library('geosphere')
library('ggplot2')
library(tidyverse)

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
  node.size.offset <- 0
  tmp_df <- data.frame(id = unique(c(edges.df$from,
                                     edges.df$to)))
  tmp_df <- tmp_df %>% inner_join(unique(df_nodes[, c('id', 'label', 'title', 
                                                      'level')]),
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
  visNetwork(nodes,
             edges,
             width = "100%")  %>%
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
                                       damping=1,
                                       springLength=100)
    ) %>%
    
    #-------------------TEMPORARILY DISABLED AS JS IS BREAKING----------------      
  visNodes(shadow=T,
           # borderWidth = 2,TEMPORARILY DISABLED
           # borderWidthSelected = 3, TEMPORARILY DISABLED
           color=list(hover=list(border='black'#,
                                 # borderWidth=3
           )))  %>%
    visEdges(
      label=edges()$title,
      font = list(size = 1)) %>%
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

#---------------------------SERVER CODE-----------------------------------------

# House Keeping Parameters---------------
mm_map_name <- ''
animate <- F
#----------------------------------------

df_nodes.original <- unique(df_nodes[, c('label', 'level', 'active', 
                                         'URL', 'endyear')]) %>%
                      arrange(label) %>% filter(label != "")
avoidLB <- F
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
  
  # DISABLING FOR THE TIME-BEING-----THIS IS NOT ALLOWING THE YEAR SLIDER TO BE UPDATED....
  # tmp.df <- reactive({
  #   if(input$range[1] != minYr | input$range[2] != maxYr)
  #   {
  #     avoidLB <<- T
  #   }
  #   else
  #     avoidLB <<- F
  #   df %>% filter(year >= input$range[1] & year <= input$range[2])
  # })


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
      filter(input$map_name  == 'All' | map_name == input$map_name) #%>%
      # dplyr::filter(year >= input$range[1] & year <= input$range[2])
    # browser()
    tmp_df <- filter_edges_mmap(tmp_df)
    tmp_df
  })
  
  # observe({
  #   updateSliderInput(session, 'range',
  #                     min=min(filtered_df()$year), max=max(filtered_df()$year),
  #                     value=c(min(filtered_df()$year), max(filtered_df()$year))
  #   )
  # })
  
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
    get_nodes(filtered_df())
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
    get_spatial_visNetwork(nodes2(), edges())
  })
  
  myVisNetworkProxy <- visNetworkProxy("networkvisfinal")
  
  # observeEvent(input$map_name, {
  #   # When a map is changed, update the year range
  #   updateSliderInput(session, 'range', value=c(min(edges()$year), 
  #                                               max(edges()$year)),
  #                     min=min(edges()$year), max=max(edges()$year))
  # })
  
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
    # browser()
    url <- unique(df_nodes[df_nodes$id==input$link_nid & 
                      df_nodes$map_name==input$map_name, "URL"])
    gname <- unique(df_nodes[df_nodes$id==input$link_nid & 
                        df_nodes$map_name==input$map_name, "label"])
    if(is.na(url))
      url <- '#'
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
        dplyr::filter(input$map_name  == 'All' | map_name == input$map_name) %>%
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
      points(x=nodes_geo()$lat, y=nodes_geo()$long, pch=19, 
             col=nodes_geo()$color, cex=2)
      
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
      
      # for(mn_idx in 1:nrow(unique.edges))
      # {
      #   coord1 <- unique.edges[mn_idx, c('longitude1', 'latitude1')]
      #   coord2 <- unique.edges[mn_idx, c('longitude2', 'latitude2')]
      #   color <- unique.edges[mn_idx, 'color']
      #   # browser()
      #   intEdges <- gcIntermediate(coord1, coord2, n=2, addStartEnd=T)
      #   lines(intEdges, col=color, lwd=2)
      # }
      longitudes <- c()
      latitudes <- c()
      # browser()
      for(i in 1:nrow(unique.edges))
      {
        lat <- c(unique.edges[i, 'latitude1'], unique.edges[i, 'latitude2'])
        lon <- c(unique.edges[i, 'longitude1'],unique.edges[i, 'longitude2'])
        lines(x=lat, y=lon, col=unique.edges[i, 'color'], lwd=2)
      }
      # browser()
      # lines(x=longitudes, y=latitudes, col=unique.edges$color, lwd=2)
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
      
      tmp.nodes <- dfs()[[1]]
      root_nodes <- tmp.nodes[tmp.nodes$root==T,]
      root_nodes <- root_nodes %>% arrange(year)
      initial_node <- root_nodes[1,]
      initial_id <- initial_node[, c('id')]
      
      visNetwork(dfs()[[1]], dfs()[[2]]) %>%
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
    
    observeEvent(input$s_map_name, {
      map_name <- input$s_map_name
      tmp.df <- df[df$map_name==map_name, c('group1_name', 'group2_name', 'year', 
                                            'label')]
      
      output$statistical_plot <- renderPlot({
        ggplot(tmp.df, aes(x=year, fill=label)) + 
          geom_bar(position='dodge', stat='count')
      })
      
    })
    
    #------------------------Administration------------------------------------
    
    # Edit Maps
    admin.maps <- reactive({
      data.frame(name=unique(df_nodes$map_name),
                 edit_links=sprintf('<a href=\"javascript:Shiny.setInputValue(\'editMap\', \'%s\');\"><i class=\"fa fa-link\"></i></a>', 
                                  unique(df_nodes$map_name)),
                 manage=sprintf('<a href=\"javascript:Shiny.setInputValue(\'manageMap\', \'%s\');\">Manage</i></a>',
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
    
    observeEvent(input$editMap, {
      
    })
    
    # Open manage a map div with information about the map populated
    observeEvent(input$manageMap, {
      mm_map_name <<- input$manageMap
      map_info <- unique(df_nodes[df_nodes$map_name==input$manageMap, 
                                                               c('map_name',
                                                                 'new_description',
                                                                 'URL',
                                                                 'level',
                                                                 'endyear')])[1,]
      session$sendCustomMessage('showEditMap', map_info)
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
      tmp.profiles <- unique(df_nodes[, c('label', 'level', 'active', 
                                          'URL', 'endyear')]) %>%
                              arrange(label) %>% filter(label != "")
      tmp.profiles$activeC <- ifelse(tmp.profiles$active==1, "Active", 
                                     tmp.profiles$endyear)
      tmp.profiles
    })

    output$ep_profiles <- renderPrint({

      profiles <- admin.profiles()
      html.table <- "<table class='admin_table' id='edit_profiles'>
                      <tr class='tr_class'>
                        <th class='th_class'>Name</th>
                        <th class='th_class'>Founded</th>
                        <th class='th_class'>Dislanded</th>
                        <th class='th_class'>Publish</th>
                        <th class='th_class'>View</th>
                        <th class='th_class'>Backup</th>
                        <th class='th_class'>Delete</th>
                      </tr>"

      html.inner <- ""
      for(i in 1:nrow(profiles))
      {
        profile <- profiles[i,]
        name <- profile$label
        active <- profile$activeC

        html.inner <- paste0(html.inner, sprintf("<tr class='tr_class'>
                                                    <td class='td_class'>%s</td>
                                                    <td class='td_class'></td>
                                                    <td class='td_class'>%s</td>
                                                    <td class='td_class'><button type='button' onclick=\"javascript:Shiny.setInputValue('publish_changes', '%s');\">Publish</button></td>
                                                    <td class='td_class'><a href=\"javascript:Shiny.setInputValue('view_profile', '%s');\"><i class=\"fa fa-eye\"></i></a></td>
                                                    <td class='td_class'><a href=\"javascript:Shiny.setInputValue('backup_profile', '%s');\">Backup</a></td>
                                                    <td class='td_class'><button type='button' onclick=\"javascript:Shiny.setInputValue('delete_profile', '%s');\">Delete</button></td>
                                                 </tr>", name, active, 
                                                 name, name, name, name))
      }
      html.inner <- paste0(html.table, paste0(html.inner, "</table>"))
      cat(html.inner)
    })
    
    observeEvent(input$delete_profile, {
      # browser()
      
      profile_name <- input$delete_profile
      row.id <- which(df_nodes.original$label == profile_name)
      session$sendCustomMessage('removeRow', row.id)
      index <- which(df_nodes.original$label==profile_name)
      df_nodes.original <<- df_nodes.original[-index,]
      
      # Delete edges before deleting the nodes
      indices <- which(df$group1_name == profile_name | df$group2_name == profile_name)
      df <<- df[-indices,]
      
      # Delete nodes
      indices <- which(df_nodes$label == profile_name)
      df_nodes <<- df_nodes[-indices,]
    })
    
    observeEvent(input$view_profile, {
      
    })
})






















