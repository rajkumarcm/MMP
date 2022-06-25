#Shiny Prototype
rm(list=ls(all=TRUE))
options(warn=-1)
#setwd("/users/irismalone/Dropbox/NCITE/MMP/prototypev2/")
library(shiny)

library(visNetwork)
library(RColorBrewer)
library(logging)
library(shinyjs)
set.seed(123)

library(logging)

source('handle_data.R')

# Load data
df <- load_data()

# Pre-process
df <- preprocess(df)

# Perhaps we do not need this as we are not making use of this anywhere
# gg <- make_graph(df)

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

# browser()
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

nodes = with(df, data.frame(id = unique(c(as.character(from),
                                          as.character(to))),
                            label = unique(c(as.character(group1_name),
                                             as.character(group2_name))),
                            title=unique(c(as.character(group1_name),
                                           as.character(group2_name))),
                            size = min(color)))
# browser() 

# Create degree centrality
# Compute the number of incoming connection for each vertex
degree_value <- degree(graph, mode = "in")

# Are both node$id and degree value the same ?
# 
# Turns out group id == degree(s) of a vertex
# Appears the group_ids were automatically created by some script.
# However I am surprised why we are doing this here given it was a script that
# generated the group_id.
# nodes$value <- degree_value[match(nodes$id, names(degree_value))]

# Throwing nodes into 5 different bins based on histogram binning technique
# on the basis of their strength represented by a unique color.
# But represented by just 4 colors for 5 bins ? I will change this to 4 for the 
# time-being

degreePal <- factor(cut(as.numeric(nodes$id), 4),
                    labels = c("lightblue", "#619CFF", "orange", "darkblue"))
nodes$central_color <- degreePal
# browser() # check what color is assigned to each group

# The amount of influence the vertex has on the flow of paths in the network
# This should be somewhat an expensive operation.
start_betweenness_value <- Sys.time()
betweenness =  betweenness(graph, directed = F) # assignment
end_betweenness_value <- Sys.time()
time_betweenness <- end_betweenness_value - start_betweenness_value

# This is not required as we are not utilising the following line
# names(betweeness)

# Magically sum(as.numeric(names(betweenness)) == as.numeric(names(degree_value)))
# shows degree_value == betweenness
# As it was the same case with degree_value I wish to comment this out as well.
nodes$between <- betweenness[match(nodes$id, names(betweenness))]

# Now betweenness is the same as degree_value and it appears 
# 'id' is an almighty here.

# Once again, three bins, but one color. This is technically correct though.
# Any rationale behind the number 3 ?
degreePal <- factor(cut(nodes$between, 3),
                    labels = c("#fde725"))
nodes$between_color <- degreePal

# Even with "with" usage, and data.frame call, there is an overhead attached to 
# it. A temporary memory is allocated until assigned back to nodes. 
# Hence I am changing it.
# nodes = with(nodes, data.frame(from, id, value, central_color, between, between_color))
# value == id hence no need to use value

nodes <- nodes[, c('id', 'central_color', 'between', 'between_color')]
# Something is not correct about by=c("from") because
# nodes$from or nodes$id is actually the degree of centrality with mode "in"
# that refers to the number of incoming edges, but we are merging with "from"
# that refers to the departing node. This must be a blender mistake I guess.
# For debugging purposes---------------------------------------------------
n_nodes1 <- length(unique(df$from))
n_nodes2 <- length(unique(as.numeric(nodes$id)))
loginfo(paste('length of df$from:', n_nodes1))
loginfo(paste('length of nodes$id:', n_nodes2))
loginfo('length(nodes$id) > length(df$from) because ids in nodes dataframe
        is a concatenation of both from and to nodes in df dataframe.')
loginfo('Pay attention at the place where you merge df and nodes')
#--------------------------------------------------------------------------
# browser()
df = merge(df, nodes, by.x=c("from"), by.y=c("id"), all.x=T)
# browser()
df_nodes <- read.csv("data/mmpgroupsfull.csv", header=T,)
df_nodes <- df_nodes[, c('group_id', 'description', 'startyear')]
cnames <- c('id', 'title', 'level')
colnames(df_nodes) <- cnames
df_nodes <- unique(df_nodes)

s <- shinyServer(function(input, output){
  
  filtered_df <- reactive({
    df %>% 
      dplyr::filter(input$map_name  == 'All' | map_name == input$map_name) %>%
      dplyr::filter(year >= input$range[1] & year <= input$range[2])
  })
  
  
  # filtered_df_nodes <-reactive({
  #   df_nodes %>%
  #     dplyr::filter(input$map_name  == 'All' | map_name == input$map_name) 
  # })
  
  edges <- reactive({
    # browser()
    data.frame(from = filtered_df()$from, 
               to = filtered_df()$to,
               source = as.character(filtered_df()$group1_name),
               target = as.character(filtered_df()$group2_name),
               # tooltip
               title=filtered_df()$title,
               #group=as.character(filtered_df()$status),
               #status=as.character(filtered_df()$status), 
               status_id=filtered_df()$status_id, 
               year=filtered_df()$year, 
               map_name=filtered_df()$map_name,
               color=filtered_df()$color
    )
  })
  
  
  nodes <- reactive({
    data.frame(id = unique(c(filtered_df()$from,
                             filtered_df()$to)),
               label = unique(c(filtered_df()$group1_name,
                                filtered_df()$group2_name)))
  })
  

  # We need this so that we can show description of each organization
  # when a vertex is hovered
  nodes2 <-reactive({merge(nodes(),
                           df_nodes,
                           by=c("id"), all.x=TRUE)
                          })
  
  # edges data.frame for legend
  tmp_df <- unique(df[, c('status', 'color')])
  ledges <- data.frame(color = tmp_df$color,
                       # str_to_title()
                       label = tmp_df$status
  )

  output$networkvisfinal <- renderVisNetwork({
    # nodes data.frame for legend
    # browser() # Check if all the values we previous saw align
    netout = visNetwork(nodes2(),
                        edges(),
                        width = "100%")  %>%

      visPhysics(solver = "repulsion") %>%
      visNodes()  %>%
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
        highlightNearest = list(enabled=T, hover=T,
                                algorithm="hierarchical",
                                degree=list(from=0, to=2)),
                           nodesIdSelection = TRUE
      )  %>%
      # visConfigure(enabled=T) %>%
      visLegend(addEdges = ledges, useGroups = FALSE)
    netout
  })
  
  myVisNetworkProxy <- visNetworkProxy("networkvisfinal")

  observe ({

    filteredEdges <- edges()[edges()$status_id %in% as.numeric(input$filterEdges), , drop = FALSE]
    filteredNodes2 <- data.frame(id=unique(c(filteredEdges$from,
                                             filteredEdges$to)),
                                 label=unique(c(as.character(filteredEdges$source),
                                                as.character(filteredEdges$target)))

                                 #title=unique(c(as.character(filteredNodes$group1_name),
                                 #              as.character(filteredNodes$group2_name)))
    )
    hiddenNodes <- anti_join(nodes(), filteredNodes2)
    hiddenEdges <- anti_join(edges(), filteredEdges)
    if(length(input$filterEdges) < max(edges()$status_id))
      if(anti_join(data.frame(id=1:5), data.frame(id=as.numeric(input$filterEdges)))$id==4)
        browser() # deliberately did this to stop only when I uncheck status_id=4

    visRemoveNodes(myVisNetworkProxy, id = hiddenNodes$id)
    visRemoveEdges(myVisNetworkProxy, id=as.character(hiddenEdges$status_id))
    visUpdateNodes(myVisNetworkProxy, nodes = filteredNodes2)

  })
  
})

