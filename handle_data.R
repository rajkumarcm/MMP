# source('preprocess_global.R', local=T)

get_latest_file <- function(path, filename)
{
  # browser()
  fnames <- list.files(path)
  dates <- str_extract(fnames, '\\d{4}_\\d{2}_\\d{2}-')
  # browser()
  dates <- str_extract(dates, '[\\d_]*')
  ymd <- str_split(dates, "_")
  dates <- NULL
  for(i in 1:length(ymd))
  {
    tmp.date <- paste(ymd[[i]], collapse='')
    dates <- c(dates, as.numeric(tmp.date))
  }
  max.date <- max(dates)
  
  year <- substr(max.date, 1, 4)
  month <- substr(max.date, 5, 6)
  date <- substr(max.date, 7, 8)
  max.date <- paste0(year, paste0("_", paste0(month, paste0("_", date))))
  
  # Get the list of file names corresponding to the latest date
  b.indices <- str_detect(fnames, sprintf('%s_%s_%s', year, month, date))
  fnames <- fnames[b.indices]
  times <- str_extract(fnames, '-\\d{2}_\\d{2}_\\d{2}')
  times <- str_extract(times, '[\\d_]+')
  hms <- str_split(times, "_")
  times <- c()
  for(i in 1:length(hms))
  {
    tmp.time <- paste(hms[[i]], collapse='')
    times <- c(times, tmp.time)
  }
  max.time <- max(times)
  if(nchar(max.time)==5)
    max.time <- sprintf("0%s",max.time)
  
  hour <- substr(max.time, 1, 2)
  mins <- substr(max.time, 3, 4)
  secs <- substr(max.time, 5, 6)
  max.time <- paste0(hour, paste0("_", paste0(mins, paste0("_", secs))))
  
  
  new.fname <- paste0(filename, paste0(max.date, paste0("-", max.time)))
  new.fname <- paste0(new.fname, '.csv')
  return(new.fname)
}

make_graph <- function(df)
{
  require(igraph)
  relations <- unique(data.frame(from=df$group1_name,
                                 to=df$group2_name))
  graph.edgelist(as.matrix(relations), directed = T)
}


load_data <- function()
{
  latest_fname <- get_latest_file('data/relationships', 'relationships')
  rel <- read.csv(paste0('data/relationships/',latest_fname), sep=',', header=T, 
                 fileEncoding = 'UTF-8-BOM', check.names=T,
                 colClasses=c('multiple'='factor'))
  # browser()
  ignore_row_idx <- which(rel$type == "")
  if(length(ignore_row_idx) >= 1)
    rel <- rel[-ignore_row_idx,]
  rel$type <- factor(rel$type, levels=unique(rel$type))
  rel
}

preprocess <- function(df)
{
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


remove_edges_ry <- function(df, undirected=T)
{
  # This function must be called after remove_edges_rd
  # Remove edges with redundant description
  
  # Get the row with least year
  get_row_lyear <- function(lids, new_df)
  {
    # If there are duplicates
    if(length(lids) > 1)
    {
      # edges with duplicates in year
      df_dyear <- df[df$link_id %in% lids,]
      
      # edges with earliest date
      df_eyear <- df_dyear[df_dyear$year == min(df_dyear$year),]
      
      new_df <- rbind(new_df, df_eyear)
    }
    else # When there are no duplicates, there will be only a single link_id
      new_df <- rbind(new_df, df[df$link_id==lids,])
    
    new_df
  }
  
  cnames_udesc <- c('from', 'to', 'status', 'map_name', 'primary')
  unique_edges <- unique(df[, cnames_udesc])
  new_df <- c()
  accepted <- c()
  
  for(i in 1:nrow(unique_edges))
  {
    d_lids <- NULL
    edge <- unique_edges[i,]
    tmp_df <- c()
    
    # tmp has link id of all duplicates of each (i.e., ith) unique edge.
    tmp <- df[df$from == edge$from & 
              df$to == edge$to & 
              df$status == edge$status & 
              df$map_name == edge$map_name & 
              df$primary == edge$primary, 'link_id']
    
    # if(245 %in% tmp)
    # {
    #   print('breakpoint...')
    # }
    tmp_forward_df <- get_row_lyear(tmp, c())
    
    reverse_lids <- df[df$from == edge$to & 
                       df$to == edge$from & 
                       df$status == edge$status & 
                       df$map_name == edge$map_name & 
                       df$primary == edge$primary, 'link_id']
    tmp_reverse_df <- get_row_lyear(reverse_lids, c())
    
    if(undirected & nrow(tmp_reverse_df) != 0)
    {
      if(tmp_forward_df$year < tmp_reverse_df$year)
      {
        if(!tmp_forward_df$link_id %in% accepted)
        {
          tmp_df <- tmp_forward_df
          accepted <- c(accepted, tmp_forward_df$link_id)
        }
        #else don't add
      }
      else
      {
        if(!tmp_reverse_df$link_id %in% accepted)
        {
          tmp_df <- tmp_reverse_df
          accepted <- c(accepted, tmp_reverse_df$link_id)
        }
      }
      
      new_df <- rbind(new_df, tmp_df)
    }
    else
    {
      if(!tmp_forward_df$link_id %in% accepted)
      {
        new_df <- rbind(new_df, tmp_forward_df)
        accepted <- c(accepted, tmp_forward_df$link_id)
      }
      
    }
    # if(nrow(new_df[new_df$link_id==245,])>0)
    # {
    #   print('breakpoint...')
    # }
  }
  
  new_df <- data.frame(new_df)
  colnames(new_df) <- colnames(df)
  new_df
}

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
    # if(row$link_id==11){
    #   print('breakpoint...')
    # }
    # Remove if exists
    
    if(length(tmp_lid) == 1)
    {
      # tmp_lid[1] is used so that we can ignore the other direction
      if((!(row$link_id %in% accepted_list)) & (!(tmp_lid[1] %in% accepted_list)))
      {
        accepted_list <- c(accepted_list, row$link_id)
      }
      #else do not add
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
      accepted_list <- c(accepted_list, row$link_id)
      count <- count + 1
    }
  }
  df[df$link_id %in% accepted_list, ]
}

clean_size_members <- function(x)
{
  x.list <- str_extract_all(x, '[\\d]+')
  x.new <- NULL
  for(i in 1:length(x.list))
  {
    item <- x.list[i]
    x.new <- c(x.new, paste0(item[1][[1]][1], ifelse(is.na(item[1][[1]][2]), "", 
                                                item[1][[1]][2])))
  }
  x.new
}

# For debugging--------------------------
# df <- load_data()
# df <- preprocess(df)
# df <- remove_edges_rd(df)
# df <- remove_edges_ry(df)
# df <- make_undirected(df)
# # df <- data.frame(from=c(0,1,1,1,2,7), to=c(1, 2, 4, 8, 1, 1))
# 
# print('debug...')
#-----------------------------------------