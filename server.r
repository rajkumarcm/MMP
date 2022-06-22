#Shiny Prototype
rm(list=ls(all=TRUE))
options(warn=-1)
#setwd("/users/irismalone/Dropbox/NCITE/MMP/prototypev2/")
library(shiny)
library(dplyr)
library(ggplot2)
library(ggiraph)

library(ggraph)
library(igraph)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(shiny)
library(ggplot2)
library(visNetwork)
library(networkD3)
library(RColorBrewer)
library(logging)
library(shinyjs)
set.seed(123)

library(ggraph)
library(igraph)
library(shinydashboard)
library(tidyverse)
library(dplyr)

library(logging)

source('handle_data.R')

# Load data
df <- load_data()

# Pre-process
df <- preprocess(df)

gg <- make_graph(df)


require(dplyr)
df = na.omit(df)

fmtarrstr <- function(arr){
  # first add ' surrounding every element
  qarr <- sprintf("'%s'",as.character(arr))
  # now concactinate them together seperated with ,
  paste(qarr,collapse=",")
}

require(RColorBrewer)
clrpal <- brewer.pal(n=11,name="Spectral")
clrscale <- sprintf('d3.scaleOrdinal() .domain([%s]) .range([%s]);',
                    fmtarrstr(1:11),fmtarrstr(clrpal))

visOptions_custom <- function (graph, width = NULL, height = NULL, highlightNearest = FALSE, 
                               nodesIdSelection = FALSE, selectedBy = NULL, autoResize = NULL, 
                               clickToUse = NULL, manipulation = NULL) 
{
  if (!any(class(graph) %in% c("visNetwork", "visNetwork_Proxy"))) {
    stop("graph must be a visNetwork or a visNetworkProxy object")
  }
  options <- list()
  options$autoResize <- autoResize
  options$clickToUse <- clickToUse
  if (is.null(manipulation)) {
    options$manipulation <- list(enabled = FALSE)
  }
  else {
    options$manipulation <- list(enabled = manipulation)
  }
  options$height <- height
  options$width <- width
  if (!is.null(manipulation)) {
    if (manipulation) {
      graph$x$datacss <- paste(readLines(system.file("htmlwidgets/lib/css/dataManipulation.css", 
                                                     package = "visNetwork"), warn = FALSE), collapse = "\n")
    }
  }
  if (!"nodes" %in% names(graph$x) && any(class(graph) %in% 
                                          "visNetwork")) {
    highlight <- list(enabled = FALSE)
    idselection <- list(enabled = FALSE)
    byselection <- list(enabled = FALSE)
  }
  else {
    highlight <- list(enabled = FALSE, hoverNearest = FALSE, 
                      degree = 1, algorithm = "all")
    if (is.list(highlightNearest)) {
      if (any(!names(highlightNearest) %in% c("enabled", 
                                              "degree", "hover", "algorithm"))) {
        stop("Invalid 'highlightNearest' argument")
      }
      if ("algorithm" %in% names(highlightNearest)) {
        stopifnot(highlightNearest$algorithm %in% c("all", 
                                                    "hierarchical"))
        highlight$algorithm <- highlightNearest$algorithm
      }
      if ("degree" %in% names(highlightNearest)) {
        highlight$degree <- highlightNearest$degree
      }
      if (highlight$algorithm %in% "hierarchical") {
        if (is.list(highlight$degree)) {
          stopifnot(all(names(highlight$degree) %in% 
                          c("from", "to")))
        }
        else {
          highlight$degree <- list(from = highlight$degree, 
                                   to = highlight$degree)
        }
      }
      if ("hover" %in% names(highlightNearest)) {
        stopifnot(is.logical(highlightNearest$hover))
        highlight$hoverNearest <- highlightNearest$hover
      }
      if ("enabled" %in% names(highlightNearest)) {
        stopifnot(is.logical(highlightNearest$enabled))
        highlight$enabled <- highlightNearest$enabled
      }
    }
    else {
      stopifnot(is.logical(highlightNearest))
      highlight$enabled <- highlightNearest
    }
    if (highlight$enabled && any(class(graph) %in% "visNetwork")) {
      if (!"label" %in% colnames(graph$x$nodes)) {
        #graph$x$nodes$label <- as.character(graph$x$nodes$id)
      }
      if (!"group" %in% colnames(graph$x$nodes)) {
        graph$x$nodes$group <- 1
      }
    }
    idselection <- list(enabled = FALSE, style = "width: 150px; height: 26px")
    if (is.list(nodesIdSelection)) {
      if (any(!names(nodesIdSelection) %in% c("enabled", 
                                              "selected", "style", "values"))) {
        stop("Invalid 'nodesIdSelection' argument. List can have 'enabled', 'selected', 'style', 'values'")
      }
      if ("selected" %in% names(nodesIdSelection)) {
        if (any(class(graph) %in% "visNetwork")) {
          if (!nodesIdSelection$selected %in% graph$x$nodes$id) {
            stop(nodesIdSelection$selected, " not in data. nodesIdSelection$selected must be valid.")
          }
        }
        idselection$selected <- nodesIdSelection$selected
      }
      if ("enabled" %in% names(nodesIdSelection)) {
        idselection$enabled <- nodesIdSelection$enabled
      }
      else {
        idselection$enabled <- TRUE
      }
      if ("style" %in% names(nodesIdSelection)) {
        idselection$style <- nodesIdSelection$style
      }
    }
    else if (is.logical(nodesIdSelection)) {
      idselection$enabled <- nodesIdSelection
    }
    else {
      stop("Invalid 'nodesIdSelection' argument")
    }
    if (idselection$enabled) {
      if ("values" %in% names(nodesIdSelection)) {
        idselection$values <- nodesIdSelection$values
        if (length(idselection$values) == 1) {
          idselection$values <- list(idselection$values)
        }
        if ("selected" %in% names(nodesIdSelection)) {
          if (!idselection$selected %in% idselection$values) {
            stop(idselection$selected, " not in data/selection. nodesIdSelection$selected must be a valid value.")
          }
        }
      }
    }
    byselection <- list(enabled = FALSE, style = "width: 150px; height: 26px", 
                        multiple = FALSE)
    if (!is.null(selectedBy)) {
      if (is.list(selectedBy)) {
        if (any(!names(selectedBy) %in% c("variable", 
                                          "selected", "style", "values", "multiple"))) {
          stop("Invalid 'selectedBy' argument. List can have 'variable', 'selected', 'style', 'values', 'multiple'")
        }
        if ("selected" %in% names(selectedBy)) {
          byselection$selected <- as.character(selectedBy$selected)
        }
        if (!"variable" %in% names(selectedBy)) {
          stop("'selectedBy' need at least 'variable' information")
        }
        byselection$variable <- selectedBy$variable
        if ("style" %in% names(selectedBy)) {
          byselection$style <- selectedBy$style
        }
        if ("multiple" %in% names(selectedBy)) {
          byselection$multiple <- selectedBy$multiple
        }
      }
      else if (is.character(selectedBy)) {
        byselection$variable <- selectedBy
      }
      else {
        stop("Invalid 'selectedBy' argument. Must a 'character' or a 'list'")
      }
      if (any(class(graph) %in% "visNetwork_Proxy")) {
        byselection$enabled <- TRUE
        if ("values" %in% names(selectedBy)) {
          byselection$values <- selectedBy$values
        }
        if ("selected" %in% names(byselection)) {
          byselection$selected <- byselection$selected
        }
      }
      else {
        if (!byselection$variable %in% colnames(graph$x$nodes)) {
          warning("Can't find '", byselection$variable, 
                  "' in node data.frame")
        }
        else {
          byselection$enabled <- TRUE
          byselection$values <- unique(graph$x$nodes[, 
                                                     byselection$variable])
          if (byselection$multiple) {
            byselection$values <- unique(gsub("^[[:space:]]*|[[:space:]]*$", 
                                              "", do.call("c", strsplit(as.character(byselection$values), 
                                                                        split = ","))))
          }
          if (any(c("integer", "numeric") %in% class(graph$x$nodes[, 
                                                                   byselection$variable]))) {
            byselection$values <- sort(byselection$values)
          }
          else {
            byselection$values <- sort(as.character(byselection$values))
          }
          if ("values" %in% names(selectedBy)) {
            byselection$values <- selectedBy$values
          }
          if ("selected" %in% names(byselection)) {
            if (!byselection$selected %in% byselection$values) {
              stop(byselection$selected, " not in data/selection. selectedBy$selected must be a valid value.")
            }
            byselection$selected <- byselection$selected
          }
          if (!"label" %in% colnames(graph$x$nodes)) {
            graph$x$nodes$label <- ""
          }
          if (!"group" %in% colnames(graph$x$nodes)) {
            graph$x$nodes$group <- 1
          }
        }
      }
    }
  }
  x <- list(highlight = highlight, idselection = idselection, 
            byselection = byselection)
  if (highlight$hoverNearest) {
    graph <- visInteraction(graph, hover = TRUE)
  }
  if (any(class(graph) %in% "visNetwork_Proxy")) {
    data <- list(id = graph$id, options = options)
    graph$session$sendCustomMessage("visShinyOptions", data)
    if (missing(highlightNearest)) {
      x$highlight <- NULL
    }
    if (missing(nodesIdSelection)) {
      x$idselection <- NULL
    }
    if (missing(selectedBy)) {
      x$byselection <- NULL
    }
    data <- list(id = graph$id, options = x)
    graph$session$sendCustomMessage("visShinyCustomOptions", 
                                    data)
  }
  else {
    graph$x <- visNetwork:::mergeLists(graph$x, x)
    graph$x$options <- visNetwork:::mergeLists(graph$x$options, options)
  }
  graph
}

filtered_df = df
relations <- data.frame(from=filtered_df$from,
                        to=filtered_df$to, 
                        status=filtered_df$status, 
                        map_name=filtered_df$map_name, 
                        year = filtered_df$year,
                        status_id = filtered_df$status,
                        title = filtered_df$title)

relations = relations[!duplicated(relations),]
relations = na.omit(relations)
relations

maps <- c("All", unique(relations$map_name))
status <- c("All", unique(relations$status))


edges <- data.frame(from = df$from, 
                    to = df$to, 
                    title=df$label,
                    group=df$status,
                    status=df$status, 
                    status_id=df$status_id, 
                    year=df$year, 
                    map=df$map,
                    color=df$color)


graph <- graph.data.frame(edges, directed = T)
degree_value <- degree(graph, mode = "in")

nodes = with(df, data.frame(id = unique(c(as.character(from),
                                          as.character(to))),
                            label = unique(c(as.character(group1_name),
                                             as.character(group2_name))),
                            title=unique(c(as.character(group1_name),
                                           as.character(group2_name))),
                            size = min(color)))

# Create degree centrality
hex <- hue_pal()(5)
df$color <- hex[df$status_id]



nodes$value <- degree_value[match(nodes$id, names(degree_value))]

degreePal <- factor(cut(nodes$value, 5),
                    labels = c("lightblue", "#619CFF", "orange", "darkblue"))
nodes$central_color <- degreePal

betweeness =  betweenness(graph, directed = F) # assignment
names(betweeness)
nodes$between <- betweeness[match(nodes$id, names(betweeness))]
#labels = c("#fde725", "#5ec962", "#21918c", "#3b528b", "#440154"))

degreePal <- factor(cut(nodes$between, 3),
                    labels = c("#fde725"))
nodes$between_color <- degreePal


nodes$from = nodes$id
nodes =with(nodes, data.frame(from, id, value, central_color, between, between_color))
df = merge(df, nodes, by=c("from"), all.x=T)

head(df)

df_nodes = read.csv("data/mmpgroupsfull.csv", header=T,)

# df_nodes$id=df_nodes$group_id
# df_nodes$title = df_nodes$description
# df_nodes$level = df_nodes$startyear
# df_nodes = with(df_nodes, data.frame(id, title, level))
# df_nodes = df_nodes[!duplicated(df_nodes),]
# head(df_nodes)
# dim(df_nodes)
# df_nodes = merge(df_nodes, nodes, by=c("id"), all.x=T)


s <- shinyServer(function(input, output){
  
  filtered_df <- reactive({
    df %>% 
      dplyr::filter(input$map_name  == 'All' | map_name == input$map_name) %>%
      dplyr::filter(year >= input$range[1] & year <= input$range[2])
  })
  
  filtered_df_nodes <-reactive({
    df_nodes %>%
      dplyr::filter(map_name == input$map_name) 
  })
  
  edges <- reactive({
    data.frame(from = filtered_df()$from, 
               to = filtered_df()$to,
               source = as.character(filtered_df()$group1_name),
               target = as.character(filtered_df()$group2_name),
               title=as.character(filtered_df()$title),
               #group=as.character(filtered_df()$status),
               #status=as.character(filtered_df()$status), 
               status_id=filtered_df()$status_id, 
               year=filtered_df()$year, 
               map_name=filtered_df()$map_name,
               color=filtered_df()$color
    )
  })
  
  nodes <- reactive({
    data.frame(id = unique(c(as.character(filtered_df()$group1_id),
                             as.character(filtered_df()$group2_id))),
               label = unique(c(as.character(filtered_df()$group1_name),
                                as.character(filtered_df()$group2_name))))
  })
  
  # browser()
  
  
  # output$networkvisfinal <- renderVisNetwork({ 
  #   # nodes data.frame for legend
  #   logging::loginfo('executing line 385')
  #   browser()
  #   netout <- visNetwork(list(unique(filtered_df_nodes$group_id)),
  #                       edges(), 
  #                       width = "100%")  %>%
  #     
  #   visPhysics(solver = "repulsion") %>%
  #   visNodes()  %>%
  #   visOptions_custom(highlightNearest = TRUE, selectedBy="label") %>%
  #   visEdges(
  #     label=edges()$title,
  #     font = list(size = 1),
  #     chosen = list(edge = TRUE,
  #                   label = htmlwidgets::JS("function(values, id, selected, hovering)
  #                                                 {values.size = 10;width=10}"))) %>%
  #     visInteraction(tooltipStyle = 'position: fixed;visibility:hidden;padding: 5px;
  #               font-family: verdana;font-size:14px;font-color:#000000;background-color: #f5f4ed;
  #               -moz-border-radius: 3px;-webkit-border-radius: 3px;border-radius: 3px;
  #                border: 1px solid #808074;white-space: wrap;box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2);
  #                max-width:300px',
  #                    hover = TRUE,
  #                    keyboard = TRUE,
  #                    dragNodes = T,
  #                    dragView = T,
  #                    zoomView = T) %>%   # explicit edge options
  #     visOptions(
  #       highlightNearest = list(enabled=T,
  #                               algorithm="hierarchical",
  #                               degree=list(from=0, to=2)),
  #       nodesIdSelection = TRUE
  #     )  %>%
  #     visConfigure(enabled=T) %>%
  #     visLegend(addEdges = ledges, useGroups = FALSE)
  #   netout
  # })
  # logging::loginfo('executing line 419')
  browser()
  nodes2 <-reactive({merge(nodes(), 
                           df_nodes, 
                           by=c("id"), all.x=TRUE) })
  
  output$networkvisfinal <- renderVisNetwork({ 
    # nodes data.frame for legend
    
    netout = visNetwork(nodes2(),
                        edges(), 
                        width = "100%")  %>%
      
      visPhysics(solver = "repulsion") %>% 
      visNodes()  %>%
      visOptions_custom(highlightNearest = TRUE, selectedBy="label") %>% 
      visEdges(
        label=edges()$title,
        font = list(size = 1),
        chosen = list(edge = TRUE, 
                      label = htmlwidgets::JS("function(values, id, selected, hovering) 
                                                    {values.size = 10;width=10}"))) %>%
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
        highlightNearest = list(enabled=T, 
                                algorithm="hierarchical", 
                                degree=list(from=0, to=2)),
        nodesIdSelection = TRUE
      )  %>%
      visConfigure(enabled=T) %>%
      visLegend(addEdges = ledges, useGroups = FALSE)
    netout
  })
  
  # myVisNetworkProxy <- visNetworkProxy("networkvisfinal")
  
})
  
  

