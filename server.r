

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
source('geocoder_helper.R')

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
      # loginfo('triggered_df triggered')
      latest.map_names <- c('All', unique(triggered_df()$map_name))
      updateSelectInput(session, 'map_name', choices=latest.map_names,
                        selected=latest.map_names[2])
      return(triggered_df())
    })
    return(tmp)
  })
  
  triggered_nodes <- reactive({
    tmp <- observe(reactive_df_node(), {
      # loginfo('reactive_df_node triggered')
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
  #  
  ledges <- reactive({
    # loginfo('ledges reactive block is triggered...')
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
                               selected=checkbox_choices)
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
    
    #  
    tmp.edges <- edges()
    tmp.edges <- tmp.edges %>% filter(year <= f_years()[1])
    tmp.nodes <- get_nodes(tmp.edges)
    if(input$animate_spatial==T)
    {
      #  
      visRemoveEdges(myVisNetworkProxy, edges()$id)
      visRemoveNodes(myVisNetworkProxy, nodes2()$id)
      visUpdateNodes(myVisNetworkProxy, tmp.nodes)
      visUpdateEdges(myVisNetworkProxy, tmp.edges)
      Sys.sleep(7)
      
      for(i in 2:length(f_years()))
      {
        #  
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
    #  
    # logging::loginfo(input$clusterNodes[['scale']])
    if(input$clusterNodes[['scale']] < 0.08 & alreadyClustered==F & 
       input$map_name=='All')
    {
      #  
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
    updateSelectInput(session, 'selectStatus', label="Highlight one status:",
                      choices=filtered_choices)
  })
  
  observeEvent(input$selectStatus, {
    selectedStatus <- as.numeric(input$selectStatus)
    if(selectedStatus > 0){
      selectedEdgesId <- edges()[edges()$status_id == selectedStatus, 'id']
      #  
      visSelectEdges(myVisNetworkProxy, selectedEdgesId)
    }
    else
      visUnselectAll(myVisNetworkProxy)
  })
  
  # For Download data
  
  observeEvent(input$nbp, {
    if(input$nbp == "downloadNM")
    {
      updateSelectInput(session, 'dd_map_name', choices=maps,
                        selected=maps[2])
    }
    # if(input$nbp=='vizNM'){
    #   updateSelectInput(session, 'map_name',
    #                     choices=maps, selected=maps[2])
    # }
    
  })
  
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
      #  
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
      
      #  
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
      #  
      unique.addresses <- unique(nodes$addr)
      rejection_list <- c()
      for(i in 1:length(unique.addresses))
      {
        addr <- unique.addresses[i]
        indices <- which(edges$g1_addr==addr & edges$g2_addr==addr)
        # We do not want these edges
        if(length(indices) > 0)
        {
          rejection_list <- c(rejection_list, indices)
        }
      }
      if(length(rejection_list) > 0)
      {
        edges <- edges[-rejection_list,] 
      }
      else
        edges
     }
    
    # Purpose of this part of the code is to provide unique edges between
    # two headquarter countries that handles both NA, empty strings, loop
    # connection and bidirectional edges.
    geo.df <- reactive({
      
      # Extract from and to IDs from the df_nodes----------------------------
      tmp.edges <- unique(df[(df$map_name==input$g_map_name |
                              input$g_map_name=='All'), c('from', 'to')] )
      #----------------------------------------------------------------------
      #  
      
      #----------------------------------------------------------------------
      # Create an edges data frame using the from, and to extracted from the
      # previous step, and join it with it's respective 
      # geo information.
      
      # For the "from" profile
      tmp.edges <- tmp.edges %>% inner_join(unique(df_nodes[, c('id', 'hq_city',
                                                                'hq_province',
                                                                'hq_country')]), 
                                            by=c('from'='id'), copy=T)
      cnames <- colnames(tmp.edges)
      cnames[cnames=='hq_city'] <- 'g1_city'
      cnames[cnames=='hq_province'] <- 'g1_province'
      cnames[cnames=='hq_country'] <- 'g1_country'
      colnames(tmp.edges) <- cnames
      tmp.edges <- drop_na(tmp.edges)
      
      # For the "to" profile
      tmp.edges <- tmp.edges %>% inner_join(unique(df_nodes[, c('id', 'hq_city',
                                                                'hq_province',
                                                                'hq_country')]), 
                                            by=c('to'='id'), copy=T)
      cnames <- colnames(tmp.edges)
      cnames[cnames=='hq_city'] <- 'g2_city'
      cnames[cnames=='hq_province'] <- 'g2_province'
      cnames[cnames=='hq_country'] <- 'g2_country'
      colnames(tmp.edges) <- cnames
      #-------------------------------------------------------------------------
      
      # Drop NA observations
      tmp.edges <- drop_na(tmp.edges)
      
      # Clone a copy of edges dataframe to nonunique.edges for other purposes
      nonunique.edges <- tmp.edges
      
      # Drop from and to that corresponds to the profile identifier
      drop.col.indices <- which(cnames %in% c('from', 'to'))
      tmp.edges <- tmp.edges[, -drop.col.indices]
      
      #-------------------------------------------------------------------------
      # Encode edges dataframe with city, province, and country information.
      # At the same time, also create nodes dataframe with info 
      # extracted from edges
      
      # For "from" profile
      tmp.edges <- unique(tmp.edges[, c('g1_city', 'g1_province', 'g1_country',
                                        'g2_city', 'g2_province', 'g2_country')])
      
      tmp.nodes1 <- unique(tmp.edges[, c('g1_city', 'g1_province', 'g1_country')])
      
      # Create an addr attribute based on city, province, and country attributes
      tmp.nodes1 <- encode_address(tmp.nodes1)
      #  
      tmp.edges <- tmp.edges %>% inner_join(tmp.nodes1, 
                                            by=c('g1_city'='hq_city',
                                                 'g1_province'='hq_province',
                                                 'g1_country'='hq_country'), 
                                            keep=F)
      cnames <- colnames(tmp.edges)
      cnames[cnames=='addr'] <- 'g1_addr'
      colnames(tmp.edges) <- cnames
      
      # For "to" profile
      tmp.nodes2 <- unique(tmp.edges[, c('g2_city', 'g2_province', 'g2_country')])
      
      # Create an addr attribute based on city, province, and country attributes
      tmp.nodes2 <- encode_address(tmp.nodes2)
      tmp.edges <- tmp.edges %>% inner_join(tmp.nodes2, 
                                            by=c('g2_city'='hq_city',
                                                 'g2_province'='hq_province',
                                                 'g2_country'='hq_country'),
                                            keep=F)
      cnames <- colnames(tmp.edges)
      cnames[cnames=='addr'] <- 'g2_addr'
      colnames(tmp.edges) <- cnames
      
      tmp.nodes <- unique(rbind(tmp.nodes1, tmp.nodes2))
      #-------------------------------------------------------------------------
      
      
      #-------------------------------------------------------------------------
      # Encode latitude, longitude information in nodes dataframe by 
      # joining tmp.nodes dataframe with lat_longs data frame
      tmp.nodes <- tmp.nodes %>% inner_join(lat_longs, by='addr')
      
      # Attach latitude, longitude for the "from" profile
      #  
      tmp.edges <- tmp.edges %>% inner_join(tmp.nodes[, c('addr', 'latitude',
                                                          'longitude')],
                                            by=c('g1_addr'='addr'))
      # Rename latitude, longitude to g1_latitude, g2_longitude
      cnames <- colnames(tmp.edges)
      cnames[cnames %in% c('latitude', 'longitude')] <- c('g1_latitude',
                                                          'g1_longitude')
      colnames(tmp.edges) <- cnames
      
      # Attach latitude, longitude for the "to" profile
      tmp.edges <- tmp.edges %>% inner_join(tmp.nodes[, c('addr', 'latitude',
                                                          'longitude')],
                                            by=c('g2_addr'='addr'))
      # Rename latitude, longitude to g2_latitude, g2_longitude
      cnames <- colnames(tmp.edges)
      cnames[cnames %in% c('latitude', 'longitude')] <- c('g2_latitude',
                                                          'g2_longitude')
      colnames(tmp.edges) <- cnames
      #-------------------------------------------------------------------------
      
      # In order to embed df_nodes with from, and to identifier information
      # first the nodes dataframe must have an id column that is auto-generated
      tmp.nodes$id <- seq(1, nrow(tmp.nodes))
      
      # Now join tmp.edges with tmp.nodes to encode group1_addr with 
      # an identifier and call it "from" and "to"
      # FROM
      tmp.edges <- tmp.edges %>% inner_join(tmp.nodes[, c('id', 'addr')], 
                                                      by=c('g1_addr'='addr'))
      cnames <- colnames(tmp.edges)
      cnames[cnames=='id'] <- 'from'
      colnames(tmp.edges) <- cnames
      
      # TO
      tmp.edges <- tmp.edges %>% inner_join(tmp.nodes[, c('id', 'addr')], 
                                                     by=c('g2_addr'='addr'))
      cnames <- colnames(tmp.edges)
      cnames[cnames=='id'] <- 'to'
      colnames(tmp.edges) <- cnames
      
      # Remove loop connection such as from=Baghdad to=Baghdad
      if(nrow(tmp.edges) > 0)
      {
        #  
        tmp.edges <- remove_loop2(tmp.nodes, tmp.edges)
      }
      
      # Before we remove bidirectional edges we need to include
      # id column that the function requires the edges data frame to work
      tmp.edges$id <- seq(1, nrow(tmp.edges))
      
      # Remove bidirectional edges such as
      # edge1: from=Baghdad to=Egypt
      # edge2: from=Egypt, to=Baghdad
      # We do not want these in edges as this would create multiple 
      # lines in geographical plot, and I think it would be nice to have
      # a single line between two distinct geolocations to elegantly display
      # the connection between them, and it's strength i.e., the number of
      # edges between them using different edge colors.
      if(nrow(tmp.edges) > 0)
      {
        tmp.edges <- remove_bidirection(tmp.edges)
      }
      
      if(nrow(tmp.edges) > 0)
      {
        #  
        return(list(tmp.edges, nonunique.edges, tmp.nodes)) # tmp.edges is unique edges
      }
      else
      {
        # Some map not have adequate information geolocation for which cases
        # we just need to return empty data frames to avoid program from
        # crashing
        return(list(data.frame(), data.frame(), data.frame()))
      }
      
    })
    
    nodes_geo <- reactive({
      
      # The goal here is to present visNetwork with nodes data frame
      # where the each node is a location, and we need color to represent
      # the amount of connection between two geographical coordinates.
      # We would like to use color to show the traffic since we are using
      # set of unique edges and using all edges could overload the geographical
      # plot with too many lines between different places and may compromise
      # interpretability. Hence, in order to keep the connections simple and
      # more transparent, unique edges are used.
      
      if(nrow(geo.df()[[1]]) > 0)
      {
        geo_unique_edges <- geo.df()[[1]]
        geo_nonunique_edges <- geo.df()[[2]]
        
        # In order to endow nodes with color, we need to first include
        # degree of centrality measure based on which a color will be allocated.
        # At this point, geo_nodes may not be able to join with df_nodes
        # since they do not share an uniquely identifiable attribute in common. 
        # However, we can utilize the location information such as city, province,
        # and country to extract edges whose "from", and "to" profiles belong to.
        # From that, we could use those IDs ("from", and "to") and create a 
        # data frame that can then be joined with df_nodes.
        
        geo_nonunique_edges <- geo_nonunique_edges %>% 
                            inner_join(geo_unique_edges[, c('g1_city',
                                                            'g1_province',
                                                            'g1_country',
                                                            'g2_city',
                                                            'g2_province',
                                                            'g2_country',
                                                            'g1_addr',
                                                            'g2_addr')],
                                       by=c('g1_city',
                                            'g1_province',
                                            'g1_country',
                                            'g2_city',
                                            'g2_province',
                                            'g2_country'))
        
        geo_nodes <- geo.df()[[3]]
        unique.addr <- geo_nodes$addr
        geo.maps <- c()
        
        for(i in 1:length(unique.addr))
        {
          addr <- unique.addr[i]
          corresponding_edges <- geo_nonunique_edges[geo_nonunique_edges$g1_addr==addr | 
                                           geo_nonunique_edges$g2_addr==addr, 
                                           c('from', 'to')]
          #  
          corresponding_node_ids <- unique(c(corresponding_edges$from, 
                                             corresponding_edges$to))
          node_values <- df_nodes[df_nodes$id %in% corresponding_node_ids, 
                                                                      'value']
          corresponding_maps <- unique(df[df$from %in% corresponding_node_ids |
                                   df$to %in% corresponding_node_ids, 'map_name'])
          geo.maps <- unique(c(geo.maps, corresponding_maps))
          
          avg_degree <- mean(node_values, na.rm=T)
          geo_nodes[geo_nodes$addr==addr, 'degree'] <- avg_degree
          
        }
        #  
        tmp.colorPalette <- colorRampPalette(c('blue', 'red'))(7)
        counts_cut <- cut(geo_nodes$degree, 7)
        node.color <- tmp.colorPalette[as.numeric(counts_cut)]
        geo_nodes$color <- node.color
        
        list(geo_nodes, geo.maps)
      }
      else
        return(data.frame(),)
      
    })
    output$geoMap <- renderPlotly({
      tmp.result <- nodes_geo()
      geo.nodes <- tmp.result[[1]]
      geo.maps <- tmp.result[[2]]
      #  
      geo.nodes <- geo.nodes[(!is.na(geo.nodes$latitude)) &
                             (!is.na(geo.nodes$longitude)),]
      geo.unique_edges <- geo.df()[[1]]
      geo.unique_edges <- geo.unique_edges[(!is.na(geo.unique_edges$g1_latitude)) &
                                           (!is.na(geo.unique_edges$g1_longitude)) &
                                           (!is.na(geo.unique_edges$g2_latitude)) &
                                           (!is.na(geo.unique_edges$g2_longitude)), ]
      
      geo.nonunique_edges <- geo.df()[[2]]
      if(nrow(geo.unique_edges) > 0 & nrow(geo.nodes) > 0)
      {
        lat_range <- range(geo.nodes$latitude, na.rm=T)
        
        lon_range <- range(geo.nodes$longitude, na.rm=T)
        
        #  
        
        # if(input$g_map_name=='All')
        # {
        #   lat_range[1] <- lat_range[1]-50 # Minimum on negative axis
        #   lat_range[2] <- lat_range[2]+50 # Maximum on positive axis
        #   lon_range[1] <- lon_range[1]-50 # Minimum on negative axis
        #   lon_range[2] <- lon_range[2]+50 # Maximum on positive axis
        #   maps::map(database="world", 
        #             border="gray10", fill=T, #bg='black', 
        #             col="#DADADA",
        #             xlim=lon_range, ylim=lat_range
        #             )  
        # }
        # else
        # {
        #   lat_range[1] <- lat_range[1]-10 # Minimum on negative axis
        #   lat_range[2] <- lat_range[2]+10 # Maximum on positive axis
        #   lon_range[1] <- lon_range[1]-10 # Minimum on negative axis
        #   lon_range[2] <- lon_range[2]+10 # Maximum on positive axis
        #   maps::map(database="world", 
        #             border="gray10", fill=T, #bg='black', 
        #             col="#DADADA",
        #             xlim=lon_range, ylim=lat_range
        #             )
        # }
        
        fig <- plot_mapbox(mode = 'scattermapbox')
        fig <- fig %>% add_markers(
          data = geo.nodes, x = ~longitude, y = ~latitude,
          text=~sprintf('%s, %s',geo.nodes$hq_city, geo.nodes$hq_province), 
          color=I("red"),
          #size = ~cnt, 
          hoverinfo = "text", alpha = 0.5, size=2) 
        
        # fig <- fig %>% add_segments(
        #   data = group_by(flights, id),
        #   x = ~start_lon, xend = ~end_lon,
        #   y = ~start_lat, yend = ~end_lat,
        #   alpha = 0.3, size = I(1), hoverinfo = "none",
        #   color=I("red")) 
        
        fig <- fig %>% layout(
          plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
          mapbox = list(style = 'dark',
                        zoom = 1.5,
                        center = list(lat = median(geo.nodes$lat),
                                      lon = median(geo.nodes$long))),
          margin = list(l = 0, r = 0,
                        b = 0, t = 0,
                        pad = 0),
          showlegend=FALSE) 
        fig <- fig %>% config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))
        
 
        
        tmp.edges <- geo.nonunique_edges # non unique edges so we can get the count of
        # connections between two hq countries
        
        # Color the edges---------------------------------------------------------
        
        geo.unique_edges$count <- 0
        for(i in 1:nrow(geo.unique_edges))
        {
          #
          m1_m2 <- geo.unique_edges[i, c('g1_addr', 'g2_addr')]
          count1 <- nrow(geo.nonunique_edges[geo.nonunique_edges$g1_addr==m1_m2$g1_addr &
                                             geo.nonunique_edges$g2_addr==m1_m2$g2_addr,])
          count2 <- nrow(geo.nonunique_edges[geo.nonunique_edges$g1_addr==m1_m2$g2_addr &
                                             geo.nonunique_edges$g2_addr==m1_m2$g1_addr,])
          count <- count1 + count2

          geo.unique_edges[geo.unique_edges$g1_addr==m1_m2$g1_addr &
                           geo.unique_edges$g2_addr==m1_m2$g2_addr, 'count'] <- count
        }

        tmp.colorPalette <- colorRampPalette(c('orange', 'red'))(7)
        count_binned <- cut(geo.unique_edges$count, 7)
        geo.unique_edges$color <- tmp.colorPalette[as.numeric(count_binned)]

        for(mn_idx in 1:nrow(geo.unique_edges))
        {
          coord1 <- geo.unique_edges[mn_idx, c('g1_longitude', 'g1_latitude')]
          coord2 <- geo.unique_edges[mn_idx, c('g2_longitude', 'g2_latitude')]
          color <- geo.unique_edges[mn_idx, 'color']
          tmp.geo.coords <- data.frame(start_lon=coord1$g1_longitude,
                                       end_lon=coord2$g2_longitude,
                                       start_lat=coord1$g1_latitude,
                                       end_lat=coord2$g2_latitude)
          #
          # intEdges <- gcIntermediate(coord1, coord2, n=1000, addStartEnd=T)
          # browser()
          # lines(intEdges, col=color, lwd=2)
          fig <- fig %>% add_segments(
                                data = tmp.geo.coords,
                                x = ~start_lon, xend = ~end_lon,
                                y = ~start_lat, yend = ~end_lat,
                                alpha = 0.3, size = I(1.3), hoverinfo = "none",
                                color=I("red")) 
        }
        
        if(input$g_map_name == "All")
        {
          #  
          # Safe time to update list of available map names in geographical plot
          # List of maps that are possible to display for geographical map
          # as some maps have no profile with valid coordinates, and this may crash 
          # the program. Hence, display map names only those that have profiles with
          # valid coordinates
          updateSelectInput(session=session, inputId="g_map_name", label="Map Name:",
                            choices=c("All", geo.maps))
        }
        fig
      }
      
    })
    
   
    
    #-------------------- Hierarchical code------------------------------------
    
    dfs <- reactive({
      
      h_edges <- df[df$map_name==input$h_map_name & (df$status=='Splinters' | 
                                                     df$status=='Mergers'), ]
      #  
      nodes_mn <- unique(c(h_edges$from, h_edges$to))
      nodes_mn <- data.frame(id=nodes_mn) %>% 
                        inner_join(df_nodes, by='id', copy=T)
      cnames <- colnames(nodes_mn)
      cnames[cnames=='level'] <- 'year'
      colnames(nodes_mn) <- cnames
      tmp.dfs <- preprocess_hdata(h_edges, nodes_mn)
      nodes_mn <- tmp.dfs[[1]]
      h_edges <- tmp.dfs[[2]]
      tmp.dfs <- get_all_done(nodes_mn, h_edges)
      nodes_mn <- tmp.dfs[[1]]
      nodes_mn<- nodes_mn %>% select(-width)
      # Inspect and understand where value is given to edges and what does this
      # attribute mean
      h_edges <- h_edges %>% select(-label)
      return(list(nodes_mn, h_edges))
    })
    
    output$reg_hideDesc2 <- renderUI({
      HTML(
        "<script>
           divs = document.getElementsByClassName('tab-pane active');
           div = null;
            for(i=0; i < divs.length; i++)
            {
                if(divs[i].getAttribute('data-value') == 'vz_hierarchical')
                    div = divs[i];
            }
           div.addEventListener('click', hideDesc2);
           div.addEventListener('click', Shiny.setInputValue('deSelectAll', Math.random()));
        </script>"
      )
    })
    
    observeEvent(input$deSelectAll, {
      h_networkProxy %>% visUnselectAll()
    })
    
    output$visnetworktimeline <- renderVisNetwork({
      tmp.edges <- dfs()[[2]]
      cnames <- colnames(tmp.edges)
      cnames[cnames=='link_id'] <- 'id'
      colnames(tmp.edges) <- cnames
      width_idx <- which(cnames=='width')
      tmp.edges <- tmp.edges[, -width_idx]
      
      tmp.nodes <- dfs()[[1]]
      browser()
      tmp.nodes <- tmp.nodes %>% inner_join(df_nodes[, c('label', 'old_description')])
      tmp.nodes <- set_border_color(tmp.nodes)
      visNetwork(tmp.nodes, tmp.edges) %>%
        visEdges(
          arrows=list(to=list(enabled=T))) %>%
        visPhysics(enabled = F) %>% 
        visEvents(type='once',
                  beforeDrawing=sprintf("function(){
                                          this.moveTo({scale:1,
                                                       position: {x:650, y:450},
                                                       });
                                          
                                         }"),#,
 
                  ) %>%
        visEvents(zoom = "function(properties){
                            Shiny.setInputValue('zoomDel', properties);
                          }",
                  dragEnd = "function(properties)
                              {
                                Shiny.setInputValue('dragDel', properties);
                              }",
                  selectEdge = "function(properties){
                                  Shiny.setInputValue('showDesc_h', 
                                  [Math.random(), properties.edges]);
                              }",
                  selectNode = "function(properties){
                                  Shiny.setInputValue('traceBackNode', properties);
                              }"
                  
                  ) %>%
        visInteraction(zoomView = F, hover=T) %>%
        visOptions(autoResize=F)
    })
    
    
    h_networkProxy <- visNetworkProxy("visnetworktimeline")
    
    # observeEvent(input$traceBackNode, {
    #   output$h_traceback <- reactive({
    #     
    #   })
    # })
    
     observeEvent(input$showDesc_h, {
       # loginfo('input$showDesc_h triggered')
       link_id <- input$showDesc_h[[2]]
       tmp <- dfs()[[2]]
       description <- tmp[tmp$link_id==link_id, 'description']
       session$sendCustomMessage('showDesc2', description)
    })
    
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
            </br>
            <svg viewBox='0 0 130 3000' xmlns='http://www.w3.org/2000/svg'>",
               paste0(svg_content, "</svg>"))
      )
    })
    

      output$h_legend_sub <- renderUI({
        
        # svg_content1 <- get_legend(ledges(), include_second_line=F)
        svg_content2 <- "
              <circle cx='20' cy='50' r='10' fill='#97C2FC' />
              <text x='60px' y='50' class='legend_label'>Original Node</text>
              <circle cx='220' cy='50' r='10' fill='#FB7E81' />
              <text x='255px' y='50' class='legend_label'>Clone Node</text>"
        # svg_content <- paste(svg_content1, svg_content2)
        svg_content <- svg_content2
        HTML(
          paste0("
          <style>
            .legend_label
            {
              font-size:15pt;
            }
          </style>
          <!--<h2 style='width:100px;'>Legend</h2>-->
          <!--</br>-->
          <svg viewBox='0 0 400 1000' xmlns='http://www.w3.org/2000/svg'>",
                 paste0(
                   svg_content, "</svg>"))
        )
      })

    
    observeEvent(input$zoomDel, {
      direction <- input$zoomDel[['direction']]
      
      session$sendCustomMessage('scaleLegend', direction)
    })
    
    observeEvent(input$dragDel, {
      deltaY <- (input$dragDel[['event']])$deltaY
      session$sendCustomMessage('moveLegend', deltaY)
    })
    
    #-------------------------Statistical Plot--------------------------------------
    
    stat_rel_df <- reactive({
      map_name <- input$s_map_name
      tmp.df <- df[df$map_name==map_name, c('group1_name', 'group2_name', 'year', 
                                            'label')]
    })
      
      growth_by_prof <- reactive({
        tmp.df <- unique(df_nodes[, c('label', 'init_size_members', 
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
        
        growth_by_profile <- tmp.df[, c('label', 'init_size_members',
                                        'max_size_members', 'between',
                                        'period')] %>%
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
        #  
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
        #  
        
        tmp.df_nodes <- NULL
        for(map_name in maps[2:length(maps)])
        {
          tmp.df <- df[df$primary==map_name, c('from', 'to')]
          tmp.nodes <- unique(c(tmp.df$from, tmp.df$to))
          tmp.nodes <- data.frame(id=tmp.nodes, map_name=map_name)
          tmp.nodes <- tmp.nodes %>% inner_join(df_nodes, by='id')
          if(is.null(tmp.df_nodes))
            tmp.df_nodes <- tmp.nodes
          else
            tmp.df_nodes <- rbind(tmp.df_nodes, data.frame(tmp.nodes))
        }
        tmp.df_nodes <- data.frame(tmp.df_nodes)
        #  
        
        ag_df <- unique(tmp.df_nodes[tmp.df_nodes$active==1 & 
                                     tmp.df_nodes$level!=0,
                                     c('label', 'map_name', 'level', 'active')]) %>% 
                 group_by(map_name, level) %>% summarise(count=n())
        ag_df <- drop_na(ag_df)
        
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
        tmp.df_nodes <- NULL
        for(map_name in maps[2:length(maps)])
        {
          tmp.df <- df[df$primary==map_name, c('from', 'to')]
          tmp.nodes <- unique(c(tmp.df$from, tmp.df$to))
          tmp.nodes <- data.frame(id=tmp.nodes, map_name=map_name)
          tmp.nodes <- tmp.nodes %>% inner_join(df_nodes, by='id')
          if(is.null(tmp.df_nodes))
            tmp.df_nodes <- tmp.nodes
          else
            tmp.df_nodes <- rbind(tmp.df_nodes, data.frame(tmp.nodes))
        }
        tmp.df_nodes <- data.frame(tmp.df_nodes)
        
        tmp.df_nodes <-unique(tmp.df_nodes[tmp.df_nodes$active==1 & 
                                           tmp.df_nodes$level!=0,
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
      
      # output$top.profiles.most.edges <- renderPlot({
      #   
      #   top_profiles <- NULL
      #   for(i in 2:length(maps))
      #   {
      #     mname <- maps[i]
      #     tmp.df_nodes <- NULL
      # 
      #     unique.nodes <- unique(df[df$primary==mname, 'label'])
      # 
      #     tmp.profiles <- NULL
      #     for(j in 1:length(unique.nodes))
      #     {
      #       node_label <- unique.nodes[j]
      #       n <- nrow(unique(df[df$group1_name == node_label |
      #                           df$group2_name == node_label,]))
      #       #  
      #       if(is.null(tmp.profiles))
      #         tmp.profiles <- data.frame(map_name=mname, label=node_label, n=n)
      #       else
      #         tmp.profiles <- rbind(top_profiles, 
      #                               data.frame(map_name=mname, label=node_label, n=n))
      #     }
      #     tmp.profiles <- tmp.profiles %>% arrange(desc(n))
      #     tmp.profiles <- tmp.profiles[1:5,]
      #     
      #     if(is.null(top_profiles))
      #       top_profiles <- tmp.profiles
      #     else
      #       top_profiles <- rbind(top_profiles, tmp.profiles)
      #   }
      #   
      #   ggplot(top_profiles, aes(x=label, y=n)) +
      #   geom_bar(stat='identity') +
      #     xlab('Profile Name') +
      #     ylab('Number of edges') +
      #     ggtitle('Number of unique profiles in each map')
      #     # theme(axis.text = element_text(size = 20, angle=90),
      #     #       plot.title = element_text(size=30),
      #     #       axis.title.x = element_text(size=20),
      #     #       axis.title.y = element_text(size=20)) +
      #     # facet_wrap(~map_name, scales='free')
      #   
      # })
      
      # For t-test
      #  
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
      #  
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
    
    observeEvent(input$new_rel_map_name, {
      maps_local <- input$new_rel_map_name
      
      if("Other" %in% maps_local)
      {
        shinyjs::enable('new_mn')
      }
      else
      {
        shinyjs::disable('new_mn')
      }
    })
    
    observeEvent(input$admin_tbsp, {
      # browser()
      if(input$admin_tbsp == "admin_nr")
      {
        updateSelectInput(session, 'new_rel_fgn', choices=profile_names,
                          selected=profile_names[1])
        updateSelectInput(session, 'new_rel_tgn', choices=profile_names,
                          selected=profile_names[1])
        updateSelectInput(session, 'new_rel_map_name', 
                          choices=c(maps[2:length(maps)], 'Other'),
                          selected=maps[2])
        updateSelectInput(session, 'new_rel_primary', 
                          choices=maps[2:length(maps)],
                          selected=maps[2])
      }
      else if(input$admin_tbsp == "admin_np")
      {
        updateSelectInput(session, 'new_prof_map', 
                          choices=c(maps[2:length(maps)], 'Other'),
                          selected=maps[2])
      }
      else if(input$admin_tbsp == "admin_ep1")
      {
        # browser()
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
            #  
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
      }
    })
    
    observeEvent(input$new_rel_mns_update, {
    
      new_mn <- str_squish(input$new_mn)
      new_mn <- str_split(new_mn, ",")[[1]]
      updateSelectInput(session, 'new_rel_primary',
                        choices=c(maps[2:length(maps)], new_mn),
                        selected=maps[2])
    })
    
    observeEvent(input$vz_tbsp, {
      if(input$vz_tbsp == "vz_spatial")
      {
        updateSelectInput(session, 'map_name',
                          choices=maps, selected=maps[2])
      }
      else if(input$vz_tbsp == "vz_hierarchical")
      {
        updateSelectInput(session, 'h_map_name', choices=maps[2:length(maps)],
                          selected=maps[2])
      }
    })
    
    observeEvent(input$newProf_schanges, {
      #  
      warnings <- c()
      end_year <- as.integer(input$newProf_schanges['ey'][[1]])
      start_year <- as.integer(input$newProf_schanges['sy'][[1]])
      init_size_year <- as.integer(input$newProf_schanges['isy'][[1]])
      init_size_members <- as.integer(input$newProf_schanges['ims'][[1]])
      max_size_members <- as.integer(input$newProf_schanges['max_sm'][[1]])
      min_size_members <- as.integer(input$newProf_schanges['msm'][[1]])
      min_size_year <- as.integer(input$newProf_schanges['msy'][[1]])
      max_size_year <- as.integer(input$newProf_schanges['max_sy'][[1]])
      
      first_attack <- as.integer(input$newProf_schanges['first_attack'][[1]])
      last_attack <- as.integer(input$newProf_schanges['last_attack'][[1]])
      last_updated <- as.integer(input$newProf_schanges['last_updated'][[1]])
      #  
      
      name <- str_trim(input$newProf_schanges['name'][[1]])
      url <- str_trim(input$newProf_schanges['url'][[1]])
      desc <- str_trim(input$newProf_schanges['desc'][[1]])
      active <- input$newProf_schanges['active'][[1]]
      complete <- input$newProf_schanges['complete'][[1]]
      
      city <- str_trim(input$newProf_schanges['city'][[1]])
      province <- str_trim(input$newProf_schanges['province'][[1]])
      country <- str_trim(input$newProf_schanges['country'][[1]])
      spons_names <- str_trim(input$newProf_schanges['spons_names'][[1]])
      spons_types <- input$new_prof_spons_types
      other_designated_names <- input$newProf_schanges['other_names'][[1]]
      comments <- input$newProf_schanges[['comments']][[1]]

      
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
      else if(tolower(name) %in% tolower(str_trim(df_nodes$label))) # duplicate check
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
      
      if(last_updated < start_year)
      {
        warnings <- c(warnings, 'Last updated cannot be before the start of the group')
      }
      if(last_attack < start_year)
      {
        warnings <- c(warnings, 'Last attack cannot be before the start of the group')
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
      
      #  
      if(length(warnings) > 0)
      {
        session$sendCustomMessage('showWarnings', 'new_prof_warnings_container')
        output$new_prof_warnings <- renderText({warnings[1]})
      }
      else
      {
        output$new_prof_warnings <- renderText({})
        session$sendCustomMessage('hideWarnings', 'new_prof_warnings_container')
        
        #  
        # Before writing the changes, check for duplicates in the df_nodes
        new.group_id <- (max(df_nodes$id)+1)
        node_record <- data.frame(group_id=new.group_id, group_name=name, 
                                  start_year=start_year, end_year=end_year,
                                  active=active, complete=complete,
                                  description=desc, on_any_map=1,
                                  page_url=url, old_description=name,
                                  last_updated=last_updated,
                                  first_attack = first_attack,
                                  last_attack = last_attack,
                                  init_size_year=init_size_year,
                                  init_size_members=init_size_members,
                                  max_size_members=max_size_members, 
                                  min_size_members=min_size_members,
                                  min_size_year = min_size_year,
                                  max_size_year = max_size_year,
                                  founding_city=city,
                                  founding_province=province,
                                  founding_country=country,
                                  founding_notes=comments,
                                  us_designated=us_sponsored,
                                  un_designated=un_sponsored,
                                  other_designated=other_designated,
                                  other_designated_names = other_designated_names,
                                  state_sponsor=state_sponsored,
                                  state_sponsor_names=spons_names,
                                  Notes=comments
                                  )
        
        # Get the file with the latest timestamp
        latest_fname <- get_latest_file('data/groups/', 'groups')
        original.df_nodes <- read.csv(paste0('data/groups/',latest_fname), 
                                      header=T,)
        
        # Append new information onto the existing dataframe
        #  
        original.df_nodes.binded <- rbind(original.df_nodes, node_record)
        
        # Get the latest date time
        time_str <- str_extract(Sys.time(), '\\d{2}:\\d{2}:\\d{2}')
        time_str <- gsub(":", "_", time_str)
        date_str <- gsub("-", "_", Sys.Date())
        date_time <- paste0(date_str, paste0('-', time_str))
        
        # Write changes to the file
        g.fname <- paste0(paste0("groups", date_time), ".csv")
        write.csv(original.df_nodes.binded, file=paste0('data/groups/', 
                                                        g.fname), row.names=F)
        
        original.df_nodes.binded <- preprocess_df_nodes(original.df_nodes.binded)
        df_nodes.full <<- original.df_nodes.binded
        
        original.df_nodes.binded$value <- 1
        original.df_nodes.binded$central_color <- NA
        original.df_nodes.binded$color.border <- 'orange'
        original.df_nodes.binded$color.highlight.background <- 'tomato'
        original.df_nodes.binded$color.hover.border <- 'orange'
        original.df_nodes.binded$color.hover.background <- 'tomato'
        original.df_nodes.binded$between <- 1
        original.df_nodes.binded$between_color <- 'yellow'
        
        # so that they are in same order
        #  
        
        profile_names <- c(profile_names, name)
        profile_names <- data.frame(x=profile_names) %>% arrange(x)
        profile_names <<- profile_names$x
        #  
        
        df_nodes <<- original.df_nodes.binded
        
        session$sendCustomMessage('sendAlert', 'New profile has been successfully added')
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
      # logging::loginfo(map_name)
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
    
    admin.profiles <- function()
    {
      tmp.profiles <- df_nodes.copy
      tmp.profiles$activeC <- ifelse(tmp.profiles$active==1, "Active", 
                                     tmp.profiles$endyear)
      tmp.profiles %>% arrange(label) %>% filter(label != "")
    }
    
    # admin.profiles <- reactive({
    #   browser()
    #   logging::loginfo(paste('rendering admin.profiles', Sys.time()))
    #   tmp.profiles <- df_nodes.copy
    #   tmp.profiles$activeC <- ifelse(tmp.profiles$active==1, "Active", 
    #                                  tmp.profiles$endyear)
    #   tmp.profiles
    # })
    
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
      browser()
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
    
    # observeEvent(input$vizNM, {
    #   if(input$vizNM == "vz_tbsp")
    #   {
    #     browser()
    #     
    # })
    
    observeEvent(input$ep_save_changes, {
      
      # Here before you apply the changes, get the anti-join between the
      # df_nodes and df_nodes.copy so that you could get the ids of the profiles
      # that you had deleted. This would enable you to retrieve the remaining
      # columns instead of saving the file with just 4 or 5 selective columns
      # and losing invaluable information.
      #  
      if(ep_changes_made==T)
      {
        browser()
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
          
          # Update profile_names after deletion
          indices <- which(profile_names %in% d.profile_names)
          profile_names <<- profile_names[-indices]
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
        write.csv(tmp.df_nodes, file=paste0('data/groups/', g.fname), 
                  row.names=F)
        write.csv(tmp.df, file=paste0('data/relationships/', r.fname), 
                  row.names=F)
        
        # Once changes are saved, then we can reset these parameters
        ep_changes_made <<- F
        
        
        # Apply the changes to the local variables for continuous use...
        # Delete profiles from df_nodes
        if(length(d.profile_names) > 0)
        {
          indices <- which(triggered_nodes()$label %in% d.profile_names)
          df_nodes <<- triggered_nodes()[-indices,]
        }
        
        # Delete edges related to the discarded profile
        indices <- which(df$group1_name %in% d.profile_names |
                         df$group2_name %in% d.profile_names)
        
        if(length(indices) > 0)
        {
          df <<- df[-indices,]
        }
        d.profile_names <<- NULL
        
        # Re-assign....
        #  
        loginfo('Inside ep_save_changes, df_nodes.copy is overwritten')
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
      # browser()
      newRel_schanges <- input$newRel_schanges
      
      group1_name <- input$new_rel_fgn
      group2_name <- input$new_rel_tgn
      map_names <- input$new_rel_map_name
      primary <- input$new_rel_primary
      status <- newRel_schanges[['type']]
      description <- newRel_schanges[['desc']]
      year <- as.integer(newRel_schanges[['year']])
      multiple <- 0
      #  
      warnings <- c()
      if(group1_name == group2_name)
      {
        warnings <- c(warnings, 'Loop connection is not allowed')
      }
      reverse_lid <- df[df$group1_name==group2_name & 
                        df$group2_name==group1_name &
                        df$status==status & df$year==year, 'link_id']
      if(length(reverse_lid) > 0)
      {
        warnings <- c(warnings, 'Reverse relationship already exists')
      }
      # browser()
      if("Other" %in% map_names)
      {
        # loginfo('Trace multiple maps behavior')
        # browser()
        new_mn <- str_squish(input$new_mn)
        new_mn <- str_split(new_mn, ",")[[1]]
        map_warnings <- F
        for(i in 1:length(new_mn))
        {
          # browser()
          if(nchar(new_mn[i]) < 3)
          {
            warnings <- c(warnings, 'New map name cannot be empty')
            map_warnings <- T
            break
          }
          else if(tolower(new_mn[i]) %in% tolower(df_nodes$map_name))
          {
            warnings <- c(warnings, 'New map name already exists')
            map_warnings <- T
            break
          }
        }
      }
      
      if(length(map_names) > 1 & "Other" %in% map_names)
      {
        map_names <- c(map_names, new_mn)
      }
      else if(length(map_names) == 1 & map_names[1] == "Other")
      {
        map_names <- new_mn
      }
      else
      {
        loginfo(paste('line number 1880: Map names: ', map_names))
      }
      map_names <- map_names[map_names != "Other"]
      # browser()
      
      group1_syear <- unique(df_nodes.full[df_nodes.full$label==group1_name, 'level'])
      group2_syear <- unique(df_nodes.full[df_nodes.full$label==group2_name, 'level'])
      group1_eyear <- unique(df_nodes.full[df_nodes.full$label==group1_name, 'endyear'])
      group2_eyear <- unique(df_nodes.full[df_nodes.full$label==group2_name, 'endyear'])
      min_year <- min(c(group1_syear, group2_syear))
      max_year <- 0
      #  
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
      
      #  
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

      #  
      
      # Check for duplicates
      for(i in 1:length(map_names))
      {
        map_name <- map_names[i]
        if(nrow(df[df$group1_name==group1_name & 
                   df$group2_name==group2_name &
                   df$map_name==map_name &
                   df$status==status &
                   df$year==year,]) > 0)
        {
          warnings <- c(warnings, 'This relationship already exists')
          break
        }
      }
      
      if(length(warnings) > 0)
      {
        session$sendCustomMessage('showWarnings', 'new_rel_warnings_container')
        output$new_rel_warnings <- renderText({warnings[1]})
      }
      else
      {
        if(!is.na(map_warnings))
        {
          maps <<- unique(c(maps, new_mn))
        }
        output$new_rel_warnings <- renderText({})
        session$sendCustomMessage('hideWarnings', 'new_rel_warnings_container')
        
        group1_id <- unique(df_nodes.full[df_nodes.full$label==group1_name, 'id'])
        group2_id <- unique(df_nodes.full[df_nodes.full$label==group2_name, 'id'])
        link_id <- max(df$link_id)+1
        link_id.backup <- link_id
        
        tmp.df2 <- NA
        for(i in 1:length(map_names))
        {
          map_name <- map_names[i]
          record <- data.frame(link_id=link_id, status=status, 
                               label=status,
                               from=group1_id, to=group2_id,
                               description=description, group1_name=group1_name,
                               group2_name=group2_name, year=year,
                               multiple=multiple, map_name=map_name,
                               primary=primary)
          # browser()
          if(anyNA(tmp.df2, recursive=T))
          {
            tmp.df2 <- record
          }
          else
            tmp.df2 <- rbind(tmp.df2, record)
          
          link_id <- link_id + 1
        }
        
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
                                paste0("An ", tmp.df2$label, " occurred in ", tmp.df2$year, 
                                       " between ", tmp.df2$group1_name, " and ", tmp.df2$group2_name),
                                paste0("A ", tmp.df2$label, " occurred in ", tmp.df2$year, 
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
        link_id <- link_id.backup
        rel_record <- NA
        for(i in 1:length(map_names))
        {
          map_name <- map_names[i]
          record <- data.frame(link_id=link_id, type=status, 
                               group1_id=group1_id, group2_id=group2_id,
                               description=description, group1_name=group1_name,
                               group2_name=group2_name, year=year,
                               multiple=multiple, map_name=map_name,
                               primary=primary)
          if(anyNA(rel_record, recursive=T))
          {
            rel_record <- record
          }
          else
            rel_record <- rbind(rel_record, record)
          link_id <- link_id + 1
        }
        
        l_rel_fname <- get_latest_file('data/relationships', 'relationships')
        tmp.df <- read.csv(paste0('data/relationships/',l_rel_fname), sep=',', 
                           header=T, fileEncoding = 'UTF-8-BOM', check.names=T,
                           colClasses=c('multiple'='factor'))
        #  
        tmp.df <- rbind(tmp.df, rel_record)
        
        time_str <- str_extract(Sys.time(), '\\d{2}:\\d{2}:\\d{2}')
        time_str <- gsub(":", "_", time_str)
        date_str <- gsub("-", "_", Sys.Date())
        date_time <- paste0(date_str, paste0('-', time_str))
        
        r.fname <- paste0(paste0("relationships", date_time), ".csv")
        write.csv(tmp.df, file=paste0('data/relationships/', r.fname), row.names=F)
        # session$sendCustomMessage('refresh_page', '')
        #  
        #  
        df <<- rbind(df, tmp.df2[, colnames(df)])
        
        tmp.df_nodes <- load_nodes_data()
        nodes <- generate_node_properties(df)
        tmp.df_nodes <- tmp.df_nodes %>% inner_join(nodes, by='id', keep=F)
        df_nodes <<- tmp.df_nodes
        
        #"""""-----------------------------------------------------------------
        #Append information onto df_nodes.copy so that edit profiles can
        #reflect on the new information, if new profiles involved.
        #"""""
        browser()
        new.profiles <- NA
        if(nrow(df_nodes.copy[df_nodes.copy$label==group1_name,])==0)
        {
          new.profiles <- df_nodes[df_nodes$label == group1_name,
                                   c('id', 'label', 'level', 'active',
                                     'URL', 'endyear')]
        }
        
        if(nrow(df_nodes.copy[df_nodes.copy$label==group2_name,])==0)
        {
          tmp.new_profile <- df_nodes[df_nodes$label == group2_name,
                                        c('id', 'label', 'level', 'active',
                                          'URL', 'endyear')]
          if(all(is.na(tmp.new_profile)))
          {
            new.profiles <- tmp.new_profile
          }
          else
            new.profiles <- rbind(new.profiles, tmp.new_profile)
        }
        
        
        tmp.df_nodes.copy <- rbind(df_nodes.copy, new.profiles)
        df_nodes.copy <<- tmp.df_nodes.copy %>% arrange(label) %>% 
                          filter(label != "")
        
        #----------------------------------------------------------------------
    
        
        
        #  
        reactive_df_node(tmp.df_nodes)
        triggered_df(df)
        session$sendCustomMessage('sendAlert', 'New edge has been successfully added')
      }
    })
    

    
})






















