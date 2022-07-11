filter_edges_mmap <- function(edges)
{
  # It is expected that redundancies by year, description are removed
  # when this function is called
  # This function aims at removing unique edges that are shown under multiple
  # map for map=All
  
  cnames <- c('from', 'to', 'status_id', 'year', 'color')
  u_edges <- unique(edges[, cnames])
  n_edges <- c()
  
  for(i in 1:nrow(u_edges))
  {
    u_e <- u_edges[i,]
    d_lids <- edges[edges$from==u_e$from &
                    edges$to==u_e$to &
                    edges$status_id==u_e$status_id &
                    edges$year==u_e$year &
                    edges$color==u_e$color,
                    'link_id']
    if(length(d_lids) > 1)
    {
      for(j in 1:length(d_lids))
        {
          d_lid <- d_lids[j]
          primary <- edges[edges$link_id==d_lid, 'primary']
          map_name <- edges[edges$link_id==d_lid, 'map_name']
          pattern <- paste0('.*', paste0(primary, '.*'))
          if(str_detect(map_name, pattern))
            n_edges <- rbind(n_edges, edges[edges$link_id==d_lid,])
        }
    }
    else
      n_edges <- rbind(n_edges, edges[edges$link_id==d_lids,])
  }
  n_edges <- data.frame(n_edges)
  colnames(n_edges) <- colnames(edges)
  n_edges
}

tmp_df <- filter_edges_mmap(df)