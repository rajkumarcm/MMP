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
  
  df$title =  ifelse(df$label=="affiliation" | df$label=="alliance", 
                     paste0("An ", df$label, " occurred in ", df$year, 
                            " between ", df$group1_name, " and ", df$group2_name),
                     paste0("A ", df$label, " occurred in ", df$year, 
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
  
  #Backup the old link_id
  df$old_link_id <- df$link_id
  
  return(df)
}

make_graph <- function(df)
{
  require(igraph)
  relations <- unique(data.frame(from=df$group1_name,
                                 to=df$group2_name))
  graph.edgelist(as.matrix(relations), directed = T)
}

