



# Perhaps we do not need this as we are not making use of this anywhere
# gg <- make_graph(df)



# fmtarrstr <- function(arr){
#   # first add ' surrounding every element
#   qarr <- sprintf("'%s'",as.character(arr))
#   # now concactinate them together seperated with ,
#   paste(qarr,collapse=",")
# }

# require(RColorBrewer)
# clrpal <- brewer.pal(n=11,name="Spectral")
# clrscale <- sprintf('d3.scaleOrdinal() .domain([%s]) .range([%s]);',
#                     fmtarrstr(1:11),fmtarrstr(clrpal))

# visOptions_custom <- function (graph, width = NULL, height = NULL, highlightNearest = FALSE,
#                                nodesIdSelection = FALSE, selectedBy = NULL, autoResize = NULL,
#                                clickToUse = NULL, manipulation = NULL)
# {
#   if (!any(class(graph) %in% c("visNetwork", "visNetwork_Proxy"))) {
#     stop("graph must be a visNetwork or a visNetworkProxy object")
#   }
#   options <- list()
#   options$autoResize <- autoResize
#   options$clickToUse <- clickToUse
#   if (is.null(manipulation)) {
#     options$manipulation <- list(enabled = FALSE)
#   }
#   else {
#     options$manipulation <- list(enabled = manipulation)
#   }
#   options$height <- height
#   options$width <- width
#   if (!is.null(manipulation)) {
#     if (manipulation) {
#       graph$x$datacss <- paste(readLines(system.file("htmlwidgets/lib/css/dataManipulation.css",
#                                                      package = "visNetwork"), warn = FALSE), collapse = "\n")
#     }
#   }
#   if (!"nodes" %in% names(graph$x) && any(class(graph) %in%
#                                           "visNetwork")) {
#     highlight <- list(enabled = FALSE)
#     idselection <- list(enabled = FALSE)
#     byselection <- list(enabled = FALSE)
#   }
#   else {
#     highlight <- list(enabled = FALSE, hoverNearest = FALSE,
#                       degree = 1, algorithm = "all")
#     if (is.list(highlightNearest)) {
#       if (any(!names(highlightNearest) %in% c("enabled",
#                                               "degree", "hover", "algorithm"))) {
#         stop("Invalid 'highlightNearest' argument")
#       }
#       if ("algorithm" %in% names(highlightNearest)) {
#         stopifnot(highlightNearest$algorithm %in% c("all",
#                                                     "hierarchical"))
#         highlight$algorithm <- highlightNearest$algorithm
#       }
#       if ("degree" %in% names(highlightNearest)) {
#         highlight$degree <- highlightNearest$degree
#       }
#       if (highlight$algorithm %in% "hierarchical") {
#         if (is.list(highlight$degree)) {
#           stopifnot(all(names(highlight$degree) %in%
#                           c("from", "to")))
#         }
#         else {
#           highlight$degree <- list(from = highlight$degree,
#                                    to = highlight$degree)
#         }
#       }
#       if ("hover" %in% names(highlightNearest)) {
#         stopifnot(is.logical(highlightNearest$hover))
#         highlight$hoverNearest <- highlightNearest$hover
#       }
#       if ("enabled" %in% names(highlightNearest)) {
#         stopifnot(is.logical(highlightNearest$enabled))
#         highlight$enabled <- highlightNearest$enabled
#       }
#     }
#     else {
#       stopifnot(is.logical(highlightNearest))
#       highlight$enabled <- highlightNearest
#     }
#     if (highlight$enabled && any(class(graph) %in% "visNetwork")) {
#       if (!"label" %in% colnames(graph$x$nodes)) {
#         #graph$x$nodes$label <- as.character(graph$x$nodes$id)
#       }
#       if (!"group" %in% colnames(graph$x$nodes)) {
#         graph$x$nodes$group <- 1
#       }
#     }
#     idselection <- list(enabled = FALSE, style = "width: 150px; height: 26px")
#     if (is.list(nodesIdSelection)) {
#       if (any(!names(nodesIdSelection) %in% c("enabled",
#                                               "selected", "style", "values"))) {
#         stop("Invalid 'nodesIdSelection' argument. List can have 'enabled', 'selected', 'style', 'values'")
#       }
#       if ("selected" %in% names(nodesIdSelection)) {
#         if (any(class(graph) %in% "visNetwork")) {
#           if (!nodesIdSelection$selected %in% graph$x$nodes$id) {
#             stop(nodesIdSelection$selected, " not in data. nodesIdSelection$selected must be valid.")
#           }
#         }
#         idselection$selected <- nodesIdSelection$selected
#       }
#       if ("enabled" %in% names(nodesIdSelection)) {
#         idselection$enabled <- nodesIdSelection$enabled
#       }
#       else {
#         idselection$enabled <- TRUE
#       }
#       if ("style" %in% names(nodesIdSelection)) {
#         idselection$style <- nodesIdSelection$style
#       }
#     }
#     else if (is.logical(nodesIdSelection)) {
#       idselection$enabled <- nodesIdSelection
#     }
#     else {
#       stop("Invalid 'nodesIdSelection' argument")
#     }
#     if (idselection$enabled) {
#       if ("values" %in% names(nodesIdSelection)) {
#         idselection$values <- nodesIdSelection$values
#         if (length(idselection$values) == 1) {
#           idselection$values <- list(idselection$values)
#         }
#         if ("selected" %in% names(nodesIdSelection)) {
#           if (!idselection$selected %in% idselection$values) {
#             stop(idselection$selected, " not in data/selection. nodesIdSelection$selected must be a valid value.")
#           }
#         }
#       }
#     }
#     byselection <- list(enabled = FALSE, style = "width: 150px; height: 26px",
#                         multiple = FALSE)
#     if (!is.null(selectedBy)) {
#       if (is.list(selectedBy)) {
#         if (any(!names(selectedBy) %in% c("variable",
#                                           "selected", "style", "values", "multiple"))) {
#           stop("Invalid 'selectedBy' argument. List can have 'variable', 'selected', 'style', 'values', 'multiple'")
#         }
#         if ("selected" %in% names(selectedBy)) {
#           byselection$selected <- as.character(selectedBy$selected)
#         }
#         if (!"variable" %in% names(selectedBy)) {
#           stop("'selectedBy' need at least 'variable' information")
#         }
#         byselection$variable <- selectedBy$variable
#         if ("style" %in% names(selectedBy)) {
#           byselection$style <- selectedBy$style
#         }
#         if ("multiple" %in% names(selectedBy)) {
#           byselection$multiple <- selectedBy$multiple
#         }
#       }
#       else if (is.character(selectedBy)) {
#         byselection$variable <- selectedBy
#       }
#       else {
#         stop("Invalid 'selectedBy' argument. Must a 'character' or a 'list'")
#       }
#       if (any(class(graph) %in% "visNetwork_Proxy")) {
#         byselection$enabled <- TRUE
#         if ("values" %in% names(selectedBy)) {
#           byselection$values <- selectedBy$values
#         }
#         if ("selected" %in% names(byselection)) {
#           byselection$selected <- byselection$selected
#         }
#       }
#       else {
#         if (!byselection$variable %in% colnames(graph$x$nodes)) {
#           warning("Can't find '", byselection$variable,
#                   "' in node data.frame")
#         }
#         else {
#           byselection$enabled <- TRUE
#           byselection$values <- unique(graph$x$nodes[,
#                                                      byselection$variable])
#           if (byselection$multiple) {
#             byselection$values <- unique(gsub("^[[:space:]]*|[[:space:]]*$",
#                                               "", do.call("c", strsplit(as.character(byselection$values),
#                                                                         split = ","))))
#           }
#           if (any(c("integer", "numeric") %in% class(graph$x$nodes[,
#                                                                    byselection$variable]))) {
#             byselection$values <- sort(byselection$values)
#           }
#           else {
#             byselection$values <- sort(as.character(byselection$values))
#           }
#           if ("values" %in% names(selectedBy)) {
#             byselection$values <- selectedBy$values
#           }
#           if ("selected" %in% names(byselection)) {
#             if (!byselection$selected %in% byselection$values) {
#               stop(byselection$selected, " not in data/selection. selectedBy$selected must be a valid value.")
#             }
#             byselection$selected <- byselection$selected
#           }
#           if (!"label" %in% colnames(graph$x$nodes)) {
#             graph$x$nodes$label <- ""
#           }
#           if (!"group" %in% colnames(graph$x$nodes)) {
#             graph$x$nodes$group <- 1
#           }
#         }
#       }
#     }
#   }
#   x <- list(highlight = highlight, idselection = idselection,
#             byselection = byselection)
#   if (highlight$hoverNearest) {
#     graph <- visInteraction(graph, hover = TRUE)
#   }
#   if (any(class(graph) %in% "visNetwork_Proxy")) {
#     data <- list(id = graph$id, options = options)
#     graph$session$sendCustomMessage("visShinyOptions", data)
#     if (missing(highlightNearest)) {
#       x$highlight <- NULL
#     }
#     if (missing(nodesIdSelection)) {
#       x$idselection <- NULL
#     }
#     if (missing(selectedBy)) {
#       x$byselection <- NULL
#     }
#     data <- list(id = graph$id, options = x)
#     graph$session$sendCustomMessage("visShinyCustomOptions",
#                                     data)
#   }
#   else {
#     graph$x <- visNetwork:::mergeLists(graph$x, x)
#     graph$x$options <- visNetwork:::mergeLists(graph$x$options, options)
#   }
#   graph
# }


s <- shinyServer(function(input, output, session){
  
  shinyjs::onclick('toggleMenu', shinyjs::showElement(id='sp', anim=T, animType='fade'))
  shinyjs::onclick('closeSp', shinyjs::toggle(id='sp', anim=T, animType='fade'))
  
  filtered_df <- reactive({
    df %>% 
      dplyr::filter(input$map_name  == 'All' | map_name == input$map_name) %>%
      dplyr::filter(year >= input$range[1] & year <= input$range[2])
  })
  
  
  edges <- reactive({
    # When edges do not have a unique identifier, they cannot be visnetwork
    # cannot be asked to remove or add them back to the network. Any form
    # of manipulation that happens without unique ID will be erroneuous. 
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
  nodes1 <- reactive({
    tmp_df <- data.frame(id = unique(c(filtered_df()$from,
                                     filtered_df()$to)),
                         label = unique(c(filtered_df()$group1_name,
                                filtered_df()$group2_name)))
     # browser()
     tmp_df <- merge(tmp_df, nodes[, c('id', 'between_color')], by='id')
     cnames <- colnames(tmp_df)
     cnames[cnames == 'between_color'] <- 'color'
     colnames(tmp_df) <- cnames
     tmp_df
  })
  
  # We need this so that we can show description of each organization
  # when a vertex is hovered
  nodes2 <-reactive({merge(nodes1(),
                           # the df that contains description, start year, etc,.
                           df_nodes,
                           by=c("id"), all.x=TRUE)
                          })
  
  # edges data.frame for legend
  tmp_df <- unique(df[, c('status', 'color')])
  
  # ledges is created & used for the sake of displaying legend
  # that aid in understanding the edges
  ledges <- data.frame(color = tmp_df$color,
                       label = tmp_df$status
  )

 
  output$networkvisfinal <- renderVisNetwork({
    
    netout = visNetwork(nodes2(),
                        edges(),
                        width = "100%")  %>%
      visPhysics(solver = "repulsion") %>%
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
        font = list(size = 1)) %>%  # SMOOTH IS FALSE FOR TESTING SPEED--------------------------
        # chosen = list(edge = TRUE,
        #               label = htmlwidgets::JS("function(values, id, selected, hovering)
        #                                             {values.size = 10;width=10}")) %>%
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
        nodesIdSelection = TRUE)  %>%
        
      # visConfigure(enabled=T) %>%
      visLegend(addEdges = ledges, useGroups = FALSE)
    netout
  })
  
  myVisNetworkProxy <- visNetworkProxy("networkvisfinal")
  
  # This observe updates the nodes and edges on visnetworkfinal - the main
  # spatial graph
  observe ({
    # loginfo(paste('Receiving edges size:', nrow(edges())))
    # loginfo(paste('Receiving nodes size:', nrow(nodes1())))
    filteredEdges <- edges()[edges()$status_id %in% as.numeric(input$filterEdges), , drop = FALSE]
    filteredNodes2 <- data.frame(id=unique(c(filteredEdges$from,
                                             filteredEdges$to)),
                                 label=unique(c(filteredEdges$source,
                                                filteredEdges$target))

    )
    hiddenNodes <- anti_join(nodes1(), filteredNodes2)
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
    fdf()
  })
  
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
        dplyr::filter(map_name == input$map_name) %>%
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
    #----------------------------------------------------------------
  
})

