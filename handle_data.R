load_data <- function()
{
  rel <- read.csv('data/relationshipsmmp.csv', sep=',', header=T, 
                 fileEncoding = 'UTF-8-BOM', check.names=T,
                 colClasses=c('multiple'='factor'))
  ignore_row_idx <- which(rel$type == "")
  rel <- rel[-ignore_row_idx,]
  rel$type <- factor(rel$type, levels=unique(rel$type))
  rel
}

preprocess <- function(df)
{
  
  # df=subset(df, df$link_id != 1803)
  cnames <- colnames(df)
  
  # Instead of creating separate columns with same information
  # that causes the memory to grow we could instead rename the columns
  # to make them more convenient
  cnames[cnames == 'type'] <- 'status' # It is already a factor
  cnames[cnames == 'group1_id'] <- 'from'
  cnames[cnames == 'group2_id'] <- 'to'
  colnames(df) <- cnames
  
  # Status in verb for displaying in title
  df$label[df$status == "Affiliates"] = "affiliation"
  df$label[df$status == "Mergers"] = "merger"
  df$label[df$status == "Splinters"] = "splinter"
  df$label[df$status == "Rivals"] = "rivalry"
  df$label[df$status == "Allies"] = "alliance"
  
  # Convert status - factor variable into their IDs.
  df$status_id = as.numeric(df$status)
  
  # For debugging-----------------------------------
  # tmp_df <- df[, c('group1_name', 'group2_name', 'status', 'status_id')]
  # bool_mask <- df$group1_name == "People's Liberation Organization of Tamil Eelam"
  # check_df <- tmp_df[bool_mask, ]
  # browser()
  #-------------------------------------------------
  
  #Backup the old link_id
  df$old_link_id <- df$link_id
  
  # link_id is not unique. Reassigning link_id to make edges unique
  df$link_id <- 1:nrow(df)
  
  df$title =  ifelse(df$label=="affiliation" | df$label=="alliance", 
                     paste0("An (lid:)", df$link_id, "", df$label, " occurred in ", df$year, 
                            " between ", df$group1_name, " and ", df$group2_name),
                     paste0("A (lid:)", df$link_id, "", df$label, " occurred in ", df$year, 
                            " between ",  df$group1_name, " and ", df$group2_name))
  require(scales)
  # Each status will have its own color.
  hex <- hue_pal()(length(unique(df$status_id)))
  df$color <- hex[df$status_id]

  #----------------------------------------------------------------------------
  df$actor_color = ifelse(df$map_name=="Global Al Qaeda" |
                          df$map_name == "Global Islamic State", 1, 0)
  #----------------------------------------------------------------------------
  
  # Remove records with missing data
  df <- na.omit(df)
  
  return(df)
}

make_graph <- function(df)
{
  require(igraph)
  relations <- unique(data.frame(from=df$group1_name,
                                 to=df$group2_name))
  graph.edgelist(as.matrix(relations), directed = T)
}

remove_edges_rd <- function(df)
{
  # Remove edges with redundant description
  cnames_uyear <- c('from', 'to', 'status', 'map_name', 'primary', 'year')
  unique_edges <- unique(df[, cnames_uyear])
  new_df <- c()
  
  for(i in 1:nrow(unique_edges))
  {
    d_lids <- NULL
    edge <- unique_edges[i,]
    
    # tmp has all duplicates of each (i.e., ith) unique edge.
    tmp <- df[df$from == edge$from & 
              df$to == edge$to & 
              df$status == edge$status & 
              df$map_name == edge$map_name & 
              df$year == edge$year &
              df$primary == edge$primary, 'link_id']
    
    # If there are duplicates
    if(length(tmp) > 1)
    {
      d_lid <- tmp[1] # Picking any one is just fine
      new_df <- rbind(new_df, df[df$link_id == d_lid,])
    }
    else
      new_df <- rbind(new_df, df[df$link_id==tmp,])
  }
  
  new_df <- data.frame(new_df)
  colnames(new_df) <- colnames(df)
  new_df
}


remove_edges_ry <- function(df)
{
  # This function must be called after remove_edges_rd
  # Remove edges with redundant description
  cnames_udesc <- c('from', 'to', 'status', 'map_name', 'primary')
  unique_edges <- unique(df[, cnames_udesc])
  new_df <- c()
  
  for(i in 1:nrow(unique_edges))
  {
    d_lids <- NULL
    edge <- unique_edges[i,]
    
    # tmp has link id of all duplicates of each (i.e., ith) unique edge.
    tmp <- df[df$from == edge$from & 
              df$to == edge$to & 
              df$status == edge$status & 
              df$map_name == edge$map_name & 
              df$primary == edge$primary, 'link_id']
    
    # If there are duplicates
    if(length(tmp) > 1)
    {
      # edges with duplicates in year
      df_dyear <- df[df$link_id %in% tmp,]
      
      # edges with earliest date
      df_eyear <- df_dyear[df_dyear$year == min(df_dyear$year),]
      
      new_df <- rbind(new_df, df_eyear)
    }
    else # When there are no duplicates, there will be only a single edge
      new_df <- rbind(new_df, df[df$link_id==tmp,])
  }
  
  new_df <- data.frame(new_df)
  colnames(new_df) <- colnames(df)
  new_df  
}

# make_undirected <- function(df)
# {
#   # When two edges with same connection i.e., 
#   # one connecting A to B, and the other connecting
#   # B to A are present, remove one of them...
#   # Do not call this if you want a directed network
#   # at which point you will see two side edges as duplicates
#   # unless there are arrows depicting the direction.
#   
#   nodes_id <- unique(c(df$from, df$to))
#   ud_df <- c()
#   visited_nodes <- c()
#   for(i in 1:length(nodes_id))
#   {
#     id <- nodes_id[i]
#     
#     # this is expected to return a unique set
#     ltor <- df[df$from==id, 'to']
#     
#     # this is NOT expected to return a unique set
#     rtol <- unique(df[df$from %in% ltor, 'to'])
#     
#     rtol <- rtol[(rtol != id) & (!(rtol %in% visited_nodes))]
#     ud_df <- rbind(ud_df, df[df$from == id,])
#     ud_df <- rbind(ud_df, df[df$from %in% rtol & df$to != id,])
#     visited_nodes <- c(visited_nodes, id)
#   }
#   ud_df <- data.frame(ud_df)
#   colnames(ud_df) <- colnames(df)
#   ud_df
# }

make_undirected <- function(df)
{
  cnames <- colnames(df)
  cnames <- cnames[-(which(cnames==c('link_id')))]
  accepted_list <- c()
  count <- 0
  for(i in 1:nrow(df))
  {
    row <- df[i, ]
    reverse_row <- row
    reverse_row$from <- row$to
    reverse_row$to <- row$from
    
    tmp_lid <- df[df$from==reverse_row$from &
                  df$to==reverse_row$to &
                  df$year==reverse_row$year &
                  # df$multiple==reverse_row$multiple &
                  df$map_name==reverse_row$map_name &
                  df$primary==reverse_row$primary &
                  df$status==reverse_row$status,
                    'link_id' # new link_id that is unique
                  ]
    # if(row$link_id==372){
    #   print('debug...')
    # }
    # Remove if exists
    
    if(length(tmp_lid) >= 1)
    {
      index <- which(df$link_id==tmp_lid)
      if(!index %in% accepted_list)
      {
        accepted_list <- c(accepted_list, i)
      }
    }
    # else if(length(tmp_lid) > 1)
    # {
    #   print('debug...')
    #   warning('more than 1 reverse row found. this should not happen...')
    # }
    else
    {
      # index <- which(df$link_id==tmp_lid)
      # if(index %in% accepted_list)
      #   {
        # print('catch me')
      # }
      accepted_list <- c(accepted_list, i)
      count <- count + 1
    }

  }
  df[accepted_list, ]
}

# For debugging--------------------------
# df <- load_data()
# df <- preprocess(df)
# df <- remove_edges_rd(df)
# df <- remove_edges_ry(df)
# # df <- data.frame(from=c(0,1,1,1,2,7), to=c(1, 2, 4, 8, 1, 1))
# df <- make_undirected(df)
# print('debug...')
#-----------------------------------------