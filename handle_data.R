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

