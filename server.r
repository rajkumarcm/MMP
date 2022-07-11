
# Note------------------------------------------------------------------------
# In the Sankey graph, connections are filtered to be either Splinters or 
# Mergers.
# dplyr::filter(status=="Splinters" | status == "Mergers")
# Why did they do this ?
#-----------------------------------------------------------------------------

library('maps')
library('geosphere')
library('ggplot2')

get_legend <- function(ledges)
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
     sub_html <- paste0(line1, paste0(text, line2))
     
     html_content <- paste(html_content, sub_html)
     y <- y + 50
   }
   html_content
}

s <- shinyServer(function(input, output, session){
  
  shinyjs::onclick('toggleMenu', shinyjs::showElement(id='sp', anim=T, animType='fade'))
  shinyjs::onclick('closeSp', shinyjs::toggle(id='sp', anim=T, animType='fade'))
  
  filtered_df <- reactive({
    df %>% 
      dplyr::filter(input$map_name  == 'All' | map_name == input$map_name) %>%
      dplyr::filter(year >= input$range[1] & year <= input$range[2])
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
    
    tmp_df <- data.frame(id = unique(c(filtered_df()$from,
                                     filtered_df()$to)))
    tmp_df <- tmp_df %>% inner_join(unique(df_nodes),
                                    by="id", keep=F)
    
    # nodes dataframe is created using correct inner join
    tmp_df <- merge(tmp_df, nodes[, c('id', 'between_color', 'value')], 
                     by='id')
    cnames <- colnames(tmp_df)
    cnames[cnames == 'between_color'] <- 'color'
    colnames(tmp_df) <- cnames
    tmp_df
  })
  
  # We need this so that we can show description of each organization
  # when a vertex is hovered
  # nodes2 <-reactive({merge(nodes1(),
  #                          # the df that contains description, start year, etc,.
  #                          df_nodes,
  #                          by=c("id"), all.x=TRUE)
  #                         })
  
  # edges data.frame for legend
  tmp_df <- unique(df[, c('status', 'color')])
  
  # ledges is created & used for the sake of displaying legend
  # that aid in understanding the edges
  ledges <- data.frame(color = tmp_df$color,
                       label = tmp_df$status
  )

 
  output$networkvisfinal <- renderVisNetwork({
    # browser()
    visNetwork(nodes2(),
                        edges(),
                        width = "100%")  %>%
      visPhysics(solver = "repulsion") %>%
      # visEvents(deselectEdge="function() {
      #           var ss = document.getElementById('selectStatus);
      #           # This is not working
      # }") %>%
      # visEvents(
#         stabilizationIterationsDone="function() {
# progrs = { 'value': params.iterations, 'total': params.total };
# shiny.setInputValue('progressBarId', progrs, {priority: 'event'});
# alert('done');
# }"
#       ) %>%
      visNodes()  %>%
      visEdges(
        label=edges()$title,
        font = list(size = 1),
        # Chosen has zero effect on the rendered labels of the edges
        chosen = list(edge = TRUE,
                      label = htmlwidgets::JS("function(values, id, selected, hovering)
                                      {alert(values); values.size = '200'; width='500px';}"))
                      ) %>%
      visInteraction(tooltipStyle = 'position: fixed;visibility:hidden;padding: 5px;
                font-family: verdana;font-size:14px;font-color:#000000;background-color: #f5f4ed;
                -moz-border-radius: 3px;-webkit-border-radius: 3px;border-radius: 3px;
                 border: 1px solid #808074;white-space: wrap;box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2);
                 max-width:300px',
                     hover = TRUE,
                     keyboard = TRUE,
                     dragNodes = T,
                     dragView = T,
                     zoomView = T) %>%   # explicit edge options
      visOptions(
        highlightNearest = list(enabled=T, #hover=T,
                                algorithm="hierarchical",
                                degree=list(from=0, to=2)),
        nodesIdSelection = TRUE,
        autoResize = T)  #%>%
        
      # visConfigure(enabled=T) %>%
      # visLegend(addEdges = ledges, useGroups = FALSE, width = '0.1', zoom = F)
    
  })
  
  myVisNetworkProxy <- visNetworkProxy("networkvisfinal")
  
  # This observe updates the nodes and edges on visnetworkfinal - the main
  # spatial graph
  observe ({
    # loginfo(paste('Receiving edges size:', nrow(edges())))
    # loginfo(paste('Receiving nodes size:', nrow(nodes1())))
    filteredEdges <- edges()[edges()$status_id %in% as.numeric(input$filterEdges), , drop = FALSE]
    filteredNodes2 <- data.frame(id=unique(c(filteredEdges$from,
                                             filteredEdges$to)))
    
    filteredNodes2 <- filteredNodes2 %>% inner_join(df_nodes[, c("id", "label")],
                                                    by="id", keep=F)
  
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
  # is checked or unchecked
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
        dplyr::filter(input$map_name  == 'All' | map_name == input$map_name) %>%
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
    
    output$nvf_legend <- renderUI({
      
      # browser()
      svg_content <- get_legend(ledges)
      HTML(
        paste0("
        <style>
        .legend_label
        {
          font-size:15pt;
        }
        </style>
        </br></br></br></br>
        <h2 style='width:100px;'>Legend</h2>
        <svg viewBox='0 0 301 500' xmlns='http://www.w3.org/2000/svg'>",
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
    #----------------------------------------------------------------
    output$geoMap <- renderPlot({
      map_name <- filtered_df()$map_name
      
      browser()
      # map("county", regions=c("new jersey"), boundary=T, col='tomato',
      #     fill=T)
      map("state", border="gray10", fill=T, bg='gray10', col="orange")
    })
    
    
})

