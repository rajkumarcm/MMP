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
set.seed(123)

library(ggraph)
library(igraph)
library(shinydashboard)
library(tidyverse)
library(dplyr)

df = read.csv("relationshipsmmp.csv")
maps <- c("All", unique(df$map_name))
status <- c("All", unique(df$type))
# Call this function with a list or vector
# Core wrapping function


#df = subset(df, df$map_name=="Global Al Qaeda" | df$map_name=="Global Islamic State"  | df$map_name=="Global Right-Wing Extremism")
df=subset(df, df$link_id != 1803) #not sure what this is
df$status=df$type
df$label[df$status == "Affiliates"] = "affiliation"
df$label[df$status == "Mergers"] = "merger"
df$label[df$status == "Splinters"] = "splinter"
df$label[df$status == "Rivals"] = "rivalry"
df$label[df$status == "Allies"] = "alliance"

#title = ifelse(df$label=="affiliation",
df$title =  ifelse(df$label=="affiliation" | df$label=="alliance", 
                   paste0("An ", df$label, " occurred in ", df$year, 
                          " between ", df$group1_name, " and ", df$group2_name),
                   paste0("A ", df$label, " occurred in ", df$year, 
                          " between ",  df$group1_name, " and ", df$group2_name))

#   writeLines(strwrap(paste0("A ", df$label, " occurred in ", df$year, " between ", 
## )


library(scales)
#extract hex color codes for a plot with three elements in ggplot2
hex <- hue_pal()(5)
#labels = c("#fde725", "#5ec962", "#21918c", "#3b528b", "#440154"))
#overlay hex color codes on actual colors
#labels = c("#fde725", "#5ec962", "#21918c", "#3b528b", "#440154"))

df$status_id = as.numeric(as.factor(df$status))
df$color = palette()[df$status_id]
df$from=df$group1_id
df$to=df$group2_id


table(I(df$group1_id==df$group2_id))
relations <- data.frame(from=df$group1_name,
                        to=df$group2_name)

relations = relations[!duplicated(relations), ]

maps <- c("All", unique(relations$map_name))
status <- c("All", unique(relations$status))

gg <- relations
netstop <- graph_from_data_frame(relations)
require(igraph)
gg <- graph.edgelist(as.matrix(relations), directed = T)

require(dplyr)

df = na.omit(df)
dim(df)


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

nodes = with(df, data.frame(id = unique(c(as.character(group1_id),
                                          as.character(group2_id))),
                            label = unique(c(as.character(group1_name),
                                             as.character(group2_name))),
                            title=unique(c(as.character(group1_name),
                                           as.character(group2_name)))))

dim(nodes)
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

nodes$group1_id = nodes$id
nodes =with(nodes, data.frame(group1_id, id, value, central_color, between, between_color))
df = merge(df, nodes, by=c("group1_id"), all.x=T)
df$actor_color = ifelse(df$map_name=="Global Al Qaeda" | df$map_name == "Global Islamic State", 1, 0)
head(df)

df_nodes = read.csv("mmpgroupsfull.csv")

df_nodes$id=df_nodes$group_id
df_nodes$title = df_nodes$description
df_nodes$level = df_nodes$startyear
df_nodes = with(df_nodes, data.frame(id, title, level))
df_nodes = df_nodes[!duplicated(df_nodes),]
head(df_nodes)
dim(df_nodes)
df_nodes = merge(df_nodes, nodes, by=c("id"), all.x=T)
head(df_nodes)
names(df_nodes)

u <- shinyUI(fluidPage(
  
  titlePanel("MMP Prototype 2"),
  
  sidebarLayout(position = "left",
                sidebarPanel( h2("Options"),
                              #selectInput("mode","Network:",c("Static","Temporal"),"Temporal"),
                              # numericInput("nnodes","Nodes:",10,1,10000,1),
                              # sliderInput("edgefak","Edge Factor:",0,10,2,step=0.1),
                              # numericInput("ngroups","Groups:",5,1,11,1),
                              textOutput("networkstat"),
                              selectInput("map_name",
                                          "Select map:",
                                          selected = maps[1],
                                          choices = c("All", unique(df$map_name))),
                              # sliderInput("edgefak","Edge Factor:",0,10,2,step=0.1),
                              
                              checkboxGroupInput("filterEdges",
                                                 "Select relationship:",
                                                 selected = df$status_id,
                                                 choices = c("Affiliates"=1, 
                                                             "Allies"=2, 
                                                             "Mergers"=3,
                                                             "Rivals"=4,
                                                             "Splinters"=5)
                                                 #choices=unique(df$status_id)
                              ),
                              
                              sliderInput("range", 
                                          label = "Choose a start and end year:",
                                          min = min(df$year), max = max(df$year), 
                                          value = c(1959, 2021),sep = "")
                              
                ),
                
                mainPanel(h2("Network Plots"),
                          tabsetPanel(
                            
                            tabPanel("Spatial",visNetworkOutput("networkvisfinal",height="500px"),
                                     style = "background-color: #eeeeee;"),
                            tabPanel("Hierarchical", visNetworkOutput("visnetworktimeline",height="500px"),
                                     style = "background-color: #eeeeee;"),
                            tabPanel("Sankey", sankeyNetworkOutput("diagram",height="500px"),
                                     style = "background-color: #eeeeee;")
                            
                          )
                )
  ),
  # WHERE YOUR FOOTER GOES
  hr(),
  print("Not for publication or distribution.")
  
  # print("Not for publication or distribution. Iris Malone (irismalone@gwu.edu)")
))


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

