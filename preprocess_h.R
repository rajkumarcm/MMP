
get_nnode <- function(id, new_year, label, new_nodes)
{
  get_fake <- function(id, new_year, label, new_nodes)
  {
    new_node <- data.frame(id=id, label=label, year=new_year)

    return(rbind(new_nodes, new_node))

  }

  # Also make sure that new_nodes does not have what we are trying to replicate
  # at the respective year
  if(!is.null(new_nodes))
  {
    if(nrow(new_nodes[new_nodes$label==label &
                      new_nodes$year==new_year,]) == 1)
    {
      new_nodes
    }
    else if(nrow(new_nodes[new_nodes$label==label &
                           new_nodes$year==new_year,]) > 1)
    {
      print('get_nnode: Alert... more than one same node exist at the same level. Inspect')
      new_nodes
    }
    else
    {
      #old_id, new_id, new_year, label, original_year, new_nodes, old_nodes
      get_fake(id, new_year, label, new_nodes)
    }
  }
  else
  {
    #old_id, new_id, new_year, label, original_year, new_nodes, old_nodes
    get_fake(id, new_year, label, new_nodes)
  }
}

check_prev <- function(nodes, label, prev_year)
{

  id <- nodes[nodes$label==label & nodes$year==prev_year, 'id']
  if(length(id) >= 1)
  {
    # i.e., if the latest year matches the current year
    # then the node does present in the nodes dataframe
    return(id)
  }
  else
    return(-1)
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

      # if(node_id==635)
      # {
      #   print('breakpoint...')
      # }

      #----------------------FROM----------------------------------------------

      prev_year <- (old_edge$year)-1
      if(i > 1)
        prev_year <- years[i-1]

      id <- check_prev(nodes, old_edge$group1_name, prev_year)
      prev_found <- F

      if(id == -1)
      {
        id <- count
        count <- count + 1
      }
      else
        prev_found <- T


      # Check if the node exist in appropriate years - FROM
      if(is.null(nodes))
      {
        nodes <- get_nnode(id, (old_edge$year)-1,
                           old_edge$group1_name, nodes)
      }
      else if(nrow(nodes[nodes$label==old_edge$group1_name &
                         nodes$year==prev_year, ])==0)
      {
        # Inserting node for the first time in the current year
        # id, new_year, label, new_nodes

        nodes <- get_nnode(id, prev_year,
                           old_edge$group1_name, nodes)

      }
      else
      {
        # Node already exists. Need to synchronise with id of prev year
        # if exists

        # id, new_year, label, new_nodes
        id <- nodes[nodes$label==old_edge$group1_name &
                      nodes$year==prev_year, 'id']
        # if(prev_found==F)
        # nodes <- get_nnode(id, (old_edge$year)-1,
        #                    old_edge$group1_name, nodes)
      }

      # Change the ID of the new edge
      new_edge$from <- id

      #----------------------TO----------------------------------------------

      # id <- check_prev(nodes, old_edge$group2_name, old_edge$year)
      # prev_found <- F
      #
      # if(id == F)
      # {
      #   id <- count
      #   count <- count + 1
      # }
      # else
      #   prev_found <- T

      # Check if the node exist in appropriate years - TO
      if(nrow(nodes[nodes$label==old_edge$group2_name &
                    nodes$year==old_edge$year, ])==0)
      {

        # Inserting node for the first time in the current year
        # id, new_year, label, new_nodes
        nodes <- get_nnode(count, old_edge$year, old_edge$group2_name, nodes)
        id <- count
        count <- count + 1

        # Removed Splinters from here
      }
      else
      {
        # Node exists and we need to ensure the ID is synchronized if
        # node with same label exist in the previous year
        id <- nodes[nodes$label==old_edge$group2_name &
                      nodes$year==old_edge$year, 'id']

        # id, new_year, label, new_nodes
        # if(prev_found==F)
        # nodes <- get_nnode(id, old_edge$year,
        #                    old_edge$group2_name, nodes)

        # Removed splinters section from here
      }

      # Change the ID of the new edge
      new_edge$to <- id

      #------------------Splinters-------------------------------------------
      first_child <- T
      if(e > 1)
      {
        
        prev_edge <- tmp_edges[(e-1),]
        if(nrow(tmp_edges[tmp_edges$from==old_edge$from &
                          tmp_edges$status=='Splinters' &
                          tmp_edges$year==old_edge$year &
                          new_edge$status=='Splinters',]) > 1)
        {
          first_child <- F
        }
      }


      if(old_edge$status=='Splinters' & first_child == T)
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

        # Just ensure duplicate node is not created
        id <- check_prev(nodes, old_edge$group1_name, old_edge$year) # +1 so it deliberately looks in the current year
        prev_found <- T

        if(id == -1)
        {
          id <- count
          count <- count + 1
        }
        else
          prev_found <- T

        # id, new_year, label, new_nodes
        nodes <- get_nnode(id=id,
                           new_year=old_edge$year,
                           label=old_edge$group1_name,
                           new_nodes=nodes)

        # Also another edge pointing from the previous node to the newly
        # created clone node
        description <- paste("Part of",
                             paste(old_edge$group1_name,
                                   "that splinted from the original group"))

        # second_edge represents the clone edge
        # browser()
        second_edge <- data.frame(from=new_edge$from, to=id,
                                  link_id=max(edges$link_id)+1, old_link_id=NaN,
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
                                  # value=old_edge$value,
                                  label=old_edge$label,
                                  width=old_edge$width)
        # browser()
        edges <- rbind(edges, second_edge)
      }

      # Modify the existing edge as to match the new node IDs
      edges[edges$from==old_edge$from &
              edges$to==old_edge$to &
              edges$year==old_edge$year &
              edges$status==old_edge$status,] <- new_edge
    }
  }


  edges <- edges %>% arrange(year)
  return(list(nodes, edges))
}

set_border_color <- function(nodes)
{
  unique.labels <- unique(nodes[nodes$clone==T, 'label'])
  
  require(RColorBrewer)
  colorPalette <- brewer.pal(length(unique.labels), 'Paired')
  # detach(RColorBrewer, unload=T)
  
  nodes$color.border <- 'black'
  nodes$borderWidth <- 3
  for(i in 1:length(unique.labels))
  {
    label <- unique.labels[i]
    color <- colorPalette[i]
    nodes[nodes$label == label, 'color.border'] <- color
  }
  nodes
  
}











































