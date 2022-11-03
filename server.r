

library('maps')
library('geosphere')
library('ggplot2')
library('gridExtra')
library('tidyverse')
library('shinyjs')

source('handle_data.R', local=T)
source('filter_medges_all.R', local=T)
source('preprocess_h.R', local=T)
source('spatial.R', local=T)
source('hierarchical.R', local=T)


#---------------------------SERVER CODE-----------------------------------------

# House Keeping Parameters---------------
mm_map_name <- ''
animate <- F
df_nodes.copy <- unique(df_nodes[, c('id', 'label', 'level', 'active', 
                                          'URL', 'endyear')]) %>%
                 arrange(label) %>% filter(label != "")
df_nodes.copy.original <- df_nodes.copy
df.copy <- df
avoidLB <- F
ep_changes_made <- F
d.profile_names <- NULL
dummyNode <- 0
#----------------------------------------

s <- shinyServer(function(input, output, session){

  # cloned version of the original df
  # This was done in order to faciliate the process of stopping the progress bar
  # from loading when the timeline slider is tweaked. Because stabilized event
  # by as a bug is failing to fire when the network reaches the stabilized point.  
  
  # initial_run parameter influences the progressbar behavior
  
  triggered_df <- reactiveVal(df)
  reactive_df_node <- reactiveVal(df_nodes)
  
  reactive_df <- reactive({
    tmp <- observe(triggered_df(), {
      loginfo('df triggered')
      return(triggered_df())
    })
    return(tmp)
  })
  
  triggered_nodes <- reactive({
    tmp <- observe(reactive_df_node(), {
      loginfo('nodes triggered')
      return(reactive_df_node())
    })
    return(tmp)
  })
  
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
    
    tmp_df <- reactive_df() %>% 
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
  })
 
  
  f_years <- reactive({
    years <- unique(edges()$year)
    years <- data.frame(year=years) %>% arrange(year)
    years <- years$year
    years
  })
  
  
  # ledges is created & used for the sake of displaying legend
  # that aid in understanding the edges
  # edges data.frame for legend
  # browser()
  ledges <- reactive({
    tmp_df <- unique(filtered_df()[, c('status', 'color')])
    data.frame(color = tmp_df$color,
               label = tmp_df$status)
  })
  

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
    # browser()
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
    # browser()
    # logging::loginfo(input$clusterNodes[['scale']])
    if(input$clusterNodes[['scale']] < 0.08 & alreadyClustered==F & 
       input$map_name=='All')
    {
      # browser()
      alreadyClustered <<- T
      session$sendCustomMessage('toggleSL', '')
      visRemoveEdges(myVisNetworkProxy, edges()$id)
      visRemoveNodes(myVisNetworkProxy, nodes2()$id)
      visUpdateNodes(myVisNetworkProxy, clustered_nodes)
      visUpdateEdges(myVisNetworkProxy, clustered_edges)
    }
    else if(input$clusterNodes[['scale']] >= 0.92 & alreadyClustered==T & 
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
    browser()
    tmp.df <- df_nodes[df_nodes$id==input$link_nid, c("URL", "label")]
    url <- tmp.df$URL
    
    if(is.na(url))
    {
      url <- "#"
    }
    else if(length(url)==0)
    {
      url <- "#"
    }
    output$link <- renderUI({
      HTML(sprintf("<a href='%s'>%s</a>", url, tmp.df$label))
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
    
    filteredNodes2 <- filteredNodes2 %>% inner_join(triggered_nodes()[, c("id", "label")],
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
      svg_content <- get_legend(ledges())
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
      tmp.edges <- tmp.edges %>% inner_join(unique(triggered_nodes()[, c('id', 'hq_country')]), 
                                            by=c('from'='id'), copy=T)
      cnames <- colnames(tmp.edges)
      cnames[cnames=='hq_country'] <- 'g1_hqc'
      colnames(tmp.edges) <- cnames
      tmp.edges <- drop_na(tmp.edges)
      tmp.edges <- tmp.edges[str_trim(tmp.edges$g1_hqc, side='both') != "",]
      
      tmp.edges <- tmp.edges %>% inner_join(unique(triggered_nodes()[, c('id', 'hq_country')]), 
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
      # browser()
      nodes_mn<- nodes_mn %>% select(-width)
      # Inspect and understand where value is given to edges and what does this
      # attribute mean
      h_edges <- h_edges %>% select(-label)
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
                                                       position: {x:650, y:450},
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
    
    reactive({
        output$h_legend_sub <- renderUI({
          # browser()
          # ledges <- dfs()[[2]]
          # ledges <- ledges[, c('status', 'color')]
          # cnames <- colnames(ledges)
          # cnames[cnames=='status'] <- 'label'
          # colnames(ledges) <- cnames
          svg_content1 <- get_legend(ledges(), include_second_line=F)
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
        tmp.df <- triggered_nodes() %>% inner_join(nodes[, c('id', 'between')], by='id')
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
      
      # output$basicStats_map <- renderPlot({
      #   tmp.df <- df[, c('group1_name', 'group2_name', 'year', 'label', 'map_name')]
      #   ggplot(tmp.df, aes(x=year, fill=label)) +
      #     geom_bar(position='dodge', stat='count') +
      #     theme(axis.text = element_text(size = 20),
      #           plot.title = element_text(size=30),
      #           axis.title.x = element_text(size=20),
      #           axis.title.y = element_text(size=20),
      #           legend.text = element_text(size=20),
      #           legend.title = element_text(size=20),
      #           legend.position = 'top') +
      #     facet_wrap(~map_name, scales='free', nrow=7)
      # })
      
      output$basicStats_map <- renderPlot({
        tmp.df <- df[!is.na(df$link_id),] %>% 
                  group_by(map_name, status) %>% 
                  summarise(count=n()) %>%
                  arrange(desc(count))
        max_count <- max(tmp.df$count)

        ggplot(tmp.df, aes(x=status, y=count)) +
          geom_bar(position='dodge', stat='identity') +
          scale_y_continuous(limits = c(0, max_count)) +
          theme(axis.text = element_text(size = 16),
                plot.title = element_text(size=20),
                axis.title.x = element_text(size=13),
                axis.title.y = element_text(size=13),
                legend.text = element_text(size=16),
                legend.title = element_text(size=16),
                legend.position = 'top') +
          facet_wrap(~map_name, scales='free', nrow=7)
      })
      
      #----------Number of active groups per year-------------------------------
      output$activeg_year <- renderPlot({

        ag_df <- unique(triggered_nodes()[triggered_nodes()$active==1 & 
                                          triggered_nodes()$level!=0 &
                                          !is.na(triggered_nodes()$map_name), 
                                 c('label', 'map_name', 'level', 'active')]) %>% 
                    
                 group_by(map_name, level) %>% summarise(count=n())
        ag_df <- drop_na(ag_df)
        # ag_df <- data.frame(ag_df) %>% filter(level != 0)
        
        ggplot(ag_df, aes(x=level, y=count, color=count)) +
          geom_point(color='steelblue', size=2) +
          geom_line(size=1) +
          xlab('Year') + ylab('Number of active groups') +
          ggtitle('Number of active groups each year') +
          scale_y_continuous(limits=c(0, 5)) +
          theme(axis.text = element_text(size = 12),
                plot.title = element_text(size=15, hjust = 0.5),
                axis.title.x = element_text(size=12),
                axis.title.y = element_text(size=12),
                legend.key.size = unit(1.3, 'cm')) +
          facet_wrap(~map_name, nrow = 6, scales='free')
      })
      
      #-------------Number of profiles per map----------------------------------
      
      output$nprofiles_map <- renderPlot({
        tmp.df_nodes <- unique(triggered_nodes()[triggered_nodes()$active==1 & 
                                                   triggered_nodes()$level!=0 &
                                          !is.na(triggered_nodes()$map_name), 
                                        c('label', 'map_name', 'level', 'active')])
        tmp.df_nodes <- tmp.df_nodes %>% group_by(map_name) %>%
                                         summarise(count=n()) %>%
                                         arrange(desc(count))
        tmp.df_nodes <- drop_na(tmp.df_nodes)
        avg_n <- mean(tmp.df_nodes$count)
        
        ggplot(tmp.df_nodes, aes(x=count, y=reorder(map_name, -count), fill=count)) +
          geom_bar(stat='identity', ) +
          geom_vline(xintercept=avg_n, color='red') +
            ylab('Map Name') +
            xlab('Number of unique profiles') +
            ggtitle('Number of unique profiles in each map') + 
            theme(axis.text = element_text(size = 16),
                  plot.title = element_text(size=20, hjust = 0.5),
                  axis.title.x = element_text(size=16),
                  axis.title.y = element_text(size=16),
                  legend.key.size = unit(2, 'cm')
                  )
        
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
          unique.nodes <- unique(triggered_nodes()[triggered_nodes()$map_name==mname, 'label'])
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
        
        ggplot(top_profiles, aes(x=label, y=n)) +
        geom_bar(stat='identity') +
          xlab('Profile Name') +
          ylab('Number of edges') +
          ggtitle('Number of unique profiles in each map')
          # theme(axis.text = element_text(size = 20, angle=90),
          #       plot.title = element_text(size=30),
          #       axis.title.x = element_text(size=20),
          #       axis.title.y = element_text(size=20)) +
          # facet_wrap(~map_name, scales='free')
        
      })
      
      # For t-test
      # browser()
      # n_edges.map_name <- df %>% group_by(primary) %>% summarise(count=n())
      output$showTtest <- renderText({
        
      })
    
    #------------------------Administration------------------------------------
    
    # Edit Maps
    admin.maps <- reactive({
      data.frame(name=unique(triggered_nodes()$map_name),
                 edit_links=sprintf('<a href=\"javascript:Shiny.setInputValue(\'editMap\', \'%s\');\"><i class=\"fa fa-link\"></i></a>', 
                                  unique(triggered_nodes()$map_name)),
                 manage=sprintf('<a href=\"javascript:Shiny.setInputValue(\'manageMap\', \'%s~@~\'+Date.now());\">Manage</i></a>',
                                unique(triggered_nodes()$map_name))
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
    
    observeEvent(input$admin_tbsp, {
      if(input$admin_tbsp == "admin_nr")
      {
        updateSelectInput(session, 'new_rel_fgn', choices=profile_names,
                          selected=profile_names[1])
        updateSelectInput(session, 'new_rel_tgn', choices=profile_names,
                          selected=profile_names[1])
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
      name <- str_trim(input$newProf_schanges['name'][[1]])
      url <- str_trim(input$newProf_schanges['url'][[1]])
      desc <- str_trim(input$newProf_schanges['desc'][[1]])
      active <- input$newProf_schanges['active'][[1]]
      complete <- input$newProf_schanges['complete'][[1]]
      map_name <- input$new_prof_map
      city <- str_trim(input$newProf_schanges['city'][[1]])
      province <- str_trim(input$newProf_schanges['province'][[1]])
      country <- str_trim(input$newProf_schanges['country'][[1]])
      spons_names <- str_trim(input$newProf_schanges['spons_names'][[1]])
      spons_types <- input$new_prof_spons_types
      comments <- input$newProf_schanges[['comments']][[1]]
      
      if(map_name == "Other")
      {
        map_name <- str_trim(input$new_mn)
        if(nchar(map_name) < 3)
          warnings <- c(warnings, 'New map name cannot be empty')
        else if(tolower(map_name) %in% tolower(triggered_nodes()$map_name))
          warnings <- c(warnings, 'New map name already exists')
      }
      
      if(nchar(city) < 3)
      {
        warnings <- c(warnings, 'City field cannot be empty')
      }
      
      if(nchar(province) < 3)
      {
        warnings <- c(warnings, 'Province cannot be empty')
      }
      
      if(nchar(country) < 3)
      {
        warnings <- c(warnings, 'Country cannot be empty')
      }
      
      if(nchar(name) < 3)
      {
        warnings <- c(warnings, 'Profile name cannot be empty')
      }
      else if(tolower(name) %in% tolower(str_trim(triggered_nodes()$label))) # duplicate check
      {
        warnings <- c(warnings, 'Profile already exists')
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
      
      if(nchar(spons_names) > 2)
      {
        if(!str_detect(spons_names, '[A-z\\-\\.\\s0-9,]*'))
        {
          warnings <- c(warnings, 'Invalid sponsor names')
        }
        
      }
      
      us_sponsored = 0
      if("US" %in% spons_types)
      {
        us_sponsored = 1
      }
      
      un_sponsored = 0
      if("UN" %in% spons_types)
      {
        un_sponsored = 1
      }
      
      state_sponsored = 0
      if("State" %in% spons_types)
      {
        state_sponsored = 1
      }
      
      other_designated = 0
      if("Others" %in% spons_types)
      {
        other_designated = 1
      }
      
      # browser()
      if(length(warnings) > 0)
      {
        session$sendCustomMessage('showWarnings', 'new_prof_warnings_container')
        output$new_prof_warnings <- renderText({warnings[1]})
      }
      else
      {
        output$new_prof_warnings <- renderText({})
        session$sendCustomMessage('hideWarnings', 'new_prof_warnings_container')
     
        # Before writing the changes, check for duplicates in the df_nodes
        node_record <- data.frame(group_id=(max(df_nodes$id)+1), group_name=name, 
                                   startyear=start_year, endyear=end_year,
                                   active=active, complete=complete,
                                   title=name, on_any_map=1, map_name=map_name,
                                   X=NA,
                                   X_merge='matched(3)', URL=url, Anchor='',
                                   description=desc, new_description=desc,
                                   href=url, 
                                   first_attack =first_attack, hq_city=city,
                                   hq_province=province, hq_country=country,
                                   init_size_members=init_size_members,
                                   max_size_members=max_size_members, 
                                   avg_size_members=NA, 
                                   us_designated=us_sponsored,
                                   un_designated=un_sponsored,
                                   other_designated=other_designated,
                                   state_sponsor=state_sponsored,
                                   state_sponsor_names=spons_names,
                                   merged='matched(3)', Notes=comments, shape='square'
                                   )
        # browser()
        
        # Get the file with the latest timestamp
        latest_fname <- get_latest_file('data/groups/', 'groups')
        latest_ge_fname <- get_latest_file('data/groups_extra/', 'groups_extra')
        
        # Append changes to the nodes file
        tmp.df_nodes <- read.csv(paste0('data/groups/',latest_fname), header=T,)
        tmp.df_nodes <- rbind(tmp.df_nodes, node_record[, c('group_id', 'group_name',
                                                            'startyear',
                                                            'endyear',
                                                            'active',
                                                            'complete',
                                                            'description',
                                                            'on_any_map',
                                                            'map_name',
                                                            'merged')])
        
        nodes_extra <<- rbind(nodes_extra, node_record[, c('group_id', 'group_name',
                                                           'startyear',
                                                           'endyear',
                                                           'active',
                                                           'complete',
                                                           'description',
                                                           'new_description',
                                                           'on_any_map',
                                                           'map_name',
                                                           'href', 'first_attack',
                                                           'hq_city', 'hq_province',
                                                           'hq_country', 
                                                           'init_size_members',
                                                           'max_size_members',
                                                           'us_designated',
                                                           'un_designated',
                                                           'other_designated',
                                                           'state_sponsor',
                                                           'state_sponsor_names',
                                                           'Notes')])
        
        time_str <- str_extract(Sys.time(), '\\d{2}:\\d{2}:\\d{2}')
        time_str <- gsub(":", "_", time_str)
        date_str <- gsub("-", "_", Sys.Date())
        date_time <- paste0(date_str, paste0('-', time_str))
        
        g.fname <- paste0(paste0("groups", date_time), ".csv")
        ge.fname <- paste0(paste0("groups_extra", date_time), ".csv")
        
        write.csv(tmp.df_nodes, file=paste0('data/groups/', g.fname), row.names=F)
        write.csv(nodes_extra, file=paste0('data/groups_extra/', ge.fname), row.names=F)
        session$sendCustomMessage('sendAlert', 'New profile has been successfully added')
        # session$sendCustomMessage('refresh_page', '')
        
        cnames <- colnames(node_record)
        cnames[cnames %in% c('group_id', 'group_name',
                             'startyear', 'X_merge')] <- c('id', 'label', 'level', 'merged')
        colnames(node_record) <- cnames
        
        # Overwrite (append changes to) df_nodes
        
        node_record$title <- node_record$label
        df_nodes <<- rbind(df_nodes, node_record[, c('id', 'label',
                                                     'level',
                                                     'endyear',
                                                     'active',
                                                     'complete',
                                                     'title',
                                                     'on_any_map', 'map_name',
                                                     'merged', 'URL', 'Anchor',
                                                     'new_description', 
                                                     'href', 'first_attack',
                                                     'hq_city', 'hq_province',
                                                     'hq_country',
                                                     'init_size_members',
                                                     'max_size_members',
                                                     'us_designated',
                                                     'un_designated',
                                                     'other_designated',
                                                     'state_sponsor',
                                                     'state_sponsor_names',
                                                     'Notes', 'shape')])
        
        # Here I have used id instead of group_id since the column names
        # gets changed already...
        nodes.temp <- data.frame(id=node_record$id, 
                                 value=min(nodes$value), 
                                 central_color='yellow',
                                 between=nodes$between[1],
                                 between_color=nodes$between_color[1],
                                 color.border=nodes$color.border[1],
                                 color.highlight.background=nodes$color.highlight.background[1],
                                 color.hover.background=nodes$color.hover.background[1],
                                 color.hover.border=nodes$color.hover.border[1])
        
        nodes <<- rbind(nodes, nodes.temp)
        
        profile_names <- c(profile_names, name)
        profile_names <- data.frame(x=profile_names) %>% arrange(x)
        profile_names <<- profile_names$x
        reactive_df_node(df_nodes)
      }

    })
    
    # Open manage a map div with information about the map populated
    observeEvent(input$manageMap, {
      mm_map_name <<- str_split(input$manageMap, '~@~')[[1]][1]
      map_info <- unique(triggered_nodes()[triggered_nodes()$map_name==mm_map_name, 
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
      nodes_under_mn <- data.frame(Profile=triggered_nodes()[triggered_nodes()$map_name==map_name, c('label')])
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
          hidden.html <- sprintf("<td class='td_class'><input type='checkbox'  onchange=\"javascript:Shiny.setInputValue('hide_profile', '%s');\" checked></td>", name) 
        }
        else
        {
          hidden.html <- sprintf("<td class='td_class'><input type='checkbox'  onchange=\"javascript:Shiny.setInputValue('hide_profile', '%s');\"></td>", name) 
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
        
        # Save hidden profile names changes-------------------------------------
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
        
        
        # Apply the changes to the local variables for continuous use...
        # Delete profiles from df_nodes
        d.profile_names
        indices <- which(triggered_nodes()$label %in% d.profile_names)
        df_nodes <<- triggered_nodes()[-indices,]
        
        # Delete edges related to the discarded profile
        indices <- which(df$group1_name %in% d.profile_names |
                         df$group2_name %in% d.profile_names)
        
        df <<- df[-indices,]
        d.profile_names <<- NULL
        
        # Re-assign....
        browser()
        df_nodes.copy <<- unique(triggered_nodes()[, c('id', 'label', 'level', 'active', 
                                                   'URL', 'endyear')])
        df_nodes.copy.original <<- df_nodes.copy
        df.copy <<- df
        triggered_df(df)
        # session$sendCustomMessage('refresh_page', '')
        
      }
    })
    
    observeEvent(input$ep_discard_changes, {
      d.profile_names <<- NULL
      ep_changes_made <<- F
      
      # Append more code when more flexibility to customization is added
    })
    
    observeEvent(input$newRel_schanges, {
      
      group1_name <- input$new_rel_fgn
      group2_name <- input$new_rel_tgn
      map_name <- input$new_rel_map_name
      primary <- input$new_rel_primary
      status <- input$newRel_schanges[['type']][[1]]
      description <- input$newRel_schanges[['desc']][[1]]
      year <- as.integer(input$newRel_schanges[['year']][[1]])
      multiple <- 0
      # browser()
      warnings <- c()
      if(group1_name == group2_name)
      {
        warnings <- c(warnings, 'Loop connection is not allowed')
      }
      
      group1_syear <- unique(triggered_nodes()[triggered_nodes()$label==group1_name, 'level'])
      group2_syear <- unique(triggered_nodes()[triggered_nodes()$label==group2_name, 'level'])
      group1_eyear <- unique(triggered_nodes()[triggered_nodes()$label==group1_name, 'endyear'])
      group2_eyear <- unique(triggered_nodes()[triggered_nodes()$label==group2_name, 'endyear'])
      min_year <- min(c(group1_syear, group2_syear))
      max_year <- 0
      if(group1_eyear == 0 & group2_eyear == 0)
      {
        max_year <- -1
      }
      else if(group1_eyear == 0)
      {
        max_year <- group2_eyear
      }
      else if(group2_eyear == 0)
      {
        max_year <- group1_eyear
      }
      else
      {
        max_year <- max(c(group1_eyear, group2_eyear))
      }
      
      # browser()
      if(max_year != -1)
      {
        if(year < min_year | year > max_year)
        {
          warnings <- c(warnings, 'Invalid year')
        } 
      }
      else
      {
        if(year < min_year)
        {
          warnings <- c(warnings, 'Invalid year')
        }
      }
      
      if(nchar(map_name) <= 2)
      {
        warnings <- c(warnings, 'map name must be supplied')
      }
      # browser()
      
      # Check for duplicates
      if(nrow(df[df$group1_name==group1_name & 
                 df$group2_name==group2_name &
                 df$map_name==map_name &
                 df$status==status &
                 df$year==year,]) > 0)
      {
        warnings <- c(warnings, 'This relationship already exists')
      }
      
      if(length(warnings) > 0)
      {
        session$sendCustomMessage('showWarnings', 'new_rel_warnings_container')
        output$new_rel_warnings <- renderText({warnings[1]})
      }
      else
      {
        output$new_rel_warnings <- renderText({})
        session$sendCustomMessage('hideWarnings', 'new_rel_warnings_container')
        
        group1_id <- unique(triggered_nodes()[triggered_nodes()$label==group1_name, 'id'])
        group2_id <- unique(triggered_nodes()[triggered_nodes()$label==group2_name, 'id'])
        link_id <- max(df$link_id)+1
        
        
        # browser()
        # label, status_id, old_link_id, title, color, actor_color, value, width
        
        # Write changes onto the edges dataframe (df)---------------------------
        tmp.df2 <- data.frame(link_id=link_id, status=status, 
                              label=status,
                              from=group1_id, to=group2_id,
                              description=description, group1_name=group1_name,
                              group2_name=group2_name, year=year,
                              multiple=multiple, map_name=map_name,
                              primary=primary)
        
        # Status in verb for displaying in title
        label <- ""
        if(status == "Affiliates")
          label <- "affiliation"
        else if(status == "Mergers")
          label <- "mergers"
        else if(status == "Splinters")
          label <- "splinters"
        else if(status == "Rivals")
          label <- "rivalry"
        else if(status == "Allies")
          label <- "alliance"
        
        tmp.df2$status_id <- unique(df[df$status==status, 'status_id'])
        tmp.df2$old_link_id <- link_id
        tmp.df2$title <- ifelse(tmp.df2$label=="affiliation" | tmp.df2$label=="alliance", 
                                paste0("An (lid:)", tmp.df2$link_id, "", tmp.df2$label, " occurred in ", tmp.df2$year, 
                                       " between ", tmp.df2$group1_name, " and ", tmp.df2$group2_name),
                                paste0("A (lid:)", tmp.df2$link_id, "", tmp.df2$label, " occurred in ", tmp.df2$year, 
                                       " between ",  tmp.df2$group1_name, " and ", tmp.df2$group2_name))
        
        require(scales)
        # Each status will have its own color.
        hex <- hue_pal()(length(unique(df$status_id)))
        tmp.df2$color <- hex[tmp.df2$status_id]
        #----------------------------------------------------------------------------
        tmp.df2$actor_color = ifelse(tmp.df2$map_name=="Global Al Qaeda" |
                                       tmp.df2$map_name == "Global Islamic State", 1, 0)
        #----------------------------------------------------------------------------
        tmp.df2$value <- 1
        tmp.df2$width <- 9 # As of now
        
        
        # Write changes onto the csv file---------------------------------------
        
        rel_record <- data.frame(link_id=link_id, type=status, 
                                 group1_id=group1_id, group2_id=group2_id,
                                 description=description, group1_name=group1_name,
                                 group2_name=group2_name, year=year,
                                 multiple=multiple, map_name=map_name,
                                 primary=primary)
        
        l_rel_fname <- get_latest_file('data/relationships', 'relationships')
        tmp.df <- read.csv(paste0('data/relationships/',l_rel_fname), sep=',', 
                           header=T, fileEncoding = 'UTF-8-BOM', check.names=T,
                           colClasses=c('multiple'='factor'))
        # browser()
        tmp.df <- rbind(tmp.df, rel_record)
        
        time_str <- str_extract(Sys.time(), '\\d{2}:\\d{2}:\\d{2}')
        time_str <- gsub(":", "_", time_str)
        date_str <- gsub("-", "_", Sys.Date())
        date_time <- paste0(date_str, paste0('-', time_str))
        
        r.fname <- paste0(paste0("relationships", date_time), ".csv")
        write.csv(tmp.df, file=paste0('data/relationships/', r.fname), row.names=F)
        session$sendCustomMessage('sendAlert', 'New edge has been successfully added')
        # session$sendCustomMessage('refresh_page', '')
        # browser()
        df <<- rbind(df, tmp.df2[, colnames(df)])
        triggered_df(df)
      }
    })
    

    
})






















