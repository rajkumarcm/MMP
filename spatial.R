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
  tmp_df <- tmp_df %>% inner_join(df_nodes[, 
                                            c('id', 'label', 'title', 
                                              'level', 'shape', 
                                              'un_designated', 
                                              'us_designated',
                                              'state_sponsor',
                                              'other_designated')],
                                  by="id", keep=F)
  
  # nodes dataframe is created using correct inner join
  tmp_df <- inner_join(tmp_df, nodes[, c('id', 'between_color', 'value', 
                                    'color.border', 'color.highlight.background',
                                    'color.hover.background',
                                    'color.hover.border')], 
                  by='id', keep=F)
  cnames <- colnames(tmp_df)
  cnames[cnames == 'between_color'] <- 'color.background'
  colnames(tmp_df) <- cnames
  tmp_df$value <- tmp_df$value + node.size.offset
  tmp_df
}

#----------------Code that inside renderVisNetwork for Spatial graph------------

get_spatial_visNetwork <- function(nodes, edges)
{
  # browser()
  legend.df <- data.frame(shape=unique(nodes$shape))
  legend.df$label <- ""
  legend.df[legend.df$shape=='star', 'label'] <- 'US or UN'
  legend.df[legend.df$shape=='diamond', 'label'] <- 'Only US'
  legend.df[legend.df$shape=='triangle', 'label'] <- 'US or UN, and State Sponsored'
  legend.df[legend.df$shape=='square', 'label'] <- 'None'
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
                                       damping=1,
                                       springLength=100)
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

filter_profiles <- function(edges)
{
  indices <- which(edges$group1_name %in% h.profile_names | 
                     edges$group2_name %in% h.profile_names)
  edges <- edges[-indices,]
  
  edges
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
        tmp.df <- rbind(tmp.df, data.frame(map_name=mname, 
                                           year=year, n_active_groups=0))
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







































