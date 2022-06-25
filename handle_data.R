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
  
  df=subset(df, df$link_id != 1803)
  cnames <- colnames(df)
  
  # Instead of creating separate columns with same information
  # that causes the memory to grow we could instead rename the columns
  # to make them more convenient
  cnames[cnames == 'type'] <- 'status' # It is already a factor
  cnames[cnames == 'group1_id'] <- 'from'
  cnames[cnames == 'group2_id'] <- 'to'
  colnames(df) <- cnames
  df$label[df$status == "Affiliates"] = "affiliation"
  df$label[df$status == "Mergers"] = "merger"
  df$label[df$status == "Splinters"] = "splinter"
  df$label[df$status == "Rivals"] = "rivalry"
  df$label[df$status == "Allies"] = "alliance"
  df$status_id = as.numeric(df$status)
  # browser()
  
  # For debugging-----------------------------------
  tmp_df <- df[, c('group1_name', 'group2_name', 'status', 'status_id')]
  bool_mask <- df$group1_name == "People's Liberation Organization of Tamil Eelam"
  check_df <- tmp_df[bool_mask, ]
  # browser()
  #-------------------------------------------------
  
  df$title =  ifelse(df$label=="affiliation" | df$label=="alliance", 
                     paste0("An ", df$label, " occurred in ", df$year, 
                            " between ", df$group1_name, " and ", df$group2_name),
                     paste0("A ", df$label, " occurred in ", df$year, 
                            " between ",  df$group1_name, " and ", df$group2_name))
  library(scales)
  # df$color = palette()[df$status_id]
  hex <- hue_pal()(5)
  df$color <- hex[df$status_id]
  return(df)
}

make_graph <- function(df)
{
  require(igraph)
  relations <- unique(data.frame(from=df$group1_name,
                                 to=df$group2_name))
  graph.edgelist(as.matrix(relations), directed = T)
}


# df <- load_data()
# df <- preprocess(df)
# 
# edges <- make_graph(df)