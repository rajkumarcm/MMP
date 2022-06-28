#Shiny Prototype
rm(list=ls(all=TRUE))
options(warn=-1)
#setwd("/users/irismalone/Dropbox/NCITE/MMP/prototypev2/")
library(shiny)

library(visNetwork)
library(networkD3)
library(RColorBrewer)
library(ggraph)
library(igraph)
library(shinyjs)
set.seed(123)

source('handle_data.R')
setwd("C:/Users/Rajkumar/Desktop/experiment/mpp/")
# Load data
df <- load_data()

# Pre-process
df <- preprocess(df)

# For the input controls
maps <- c("All", unique(df$map_name))
status_names <- as.character(unique(df$status))
status_id <- 


gg <- make_graph(df)

# require(dplyr)

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

nodes = with(df, data.frame(id = unique(c(as.character(from),
                                          as.character(to))),
                            label = unique(c(as.character(group1_name),
                                             as.character(group2_name))),
                            title=unique(c(as.character(group1_name),
                                           as.character(group2_name)))))

# Create degree centrality
nodes$value <- degree_value[match(nodes$id, names(degree_value))]

# logging::loginfo(length(nodes$value))
# logging::loginfo('Passed 117 line')
degreePal <- factor(cut(nodes$value, 5),
                    labels = c("lightblue", "#619CFF", "orange", "darkblue"))
# logging::loginfo('Passed 121 line')
nodes$central_color <- degreePal

betweeness =  betweenness(graph, directed = F) # assignment
names(betweeness)
nodes$between <- betweeness[match(nodes$id, names(betweeness))]
#labels = c("#fde725", "#5ec962", "#21918c", "#3b528b", "#440154"))

degreePal <- factor(cut(nodes$between, 3),
                    labels = c("#fde725"))
nodes$between_color <- degreePal

nodes$from = nodes$id
nodes =with(nodes, data.frame(from, id, value, central_color, between,
                              between_color))
df = merge(df, nodes, by=c("from"), all.x=T)
df$actor_color = ifelse(df$map_name=="Global Al Qaeda" | 
                          df$map_name == "Global Islamic State", 1, 0)
head(df)

u <- shinyUI(fluidPage(
  
  
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      #sp
      {
        width:480px;
        height:800px;
        position:absolute;
        z-index:100;
        top:200px;
        background-color:#d9d9d9;
      }
      
      #sp2
      {
        position:relative;
        width:400px;
        height:800px;
        margin-left:80px
      }
      
      #icon_div
      {
        width:20px;
        height:20px;
        margin-top:10px;
        margin-left:370px;
      }
      
      #mp
      {
        margin-left:85px;
        top:87px;
      }
      
      body{
         background-color:#eeeeee;
         width:2560px;
         height:1440;
      }
      
      #range
      {
        font-size:5pt;
      }
      
      .irs-grid-text{
        font-size: 12pt;
        transform: rotate(-90deg) translate(-30px);"
      ))
  ),
  
  shinyjs::useShinyjs(),
  titlePanel("MMP Prototype 2"),
  
  sidebarLayout(position = "left",
                shinyjs::hidden(
                div( id="sp",
                     div(id="sp2", 
                     div(id ="icon_div",
                         tags$img(id="closeSp", height = 20, width = 20, 
                                  src = 'close.png') ),
                              h2("Options"),
                              selectInput("map_name",
                                          "Select map:",
                                          selected = maps[1],
                                          choices = maps),
                              
                              checkboxGroupInput("filterEdges",
                                                 "Select relationship:",
                                                 selected = unique(df$status_id),
                                                 choices = c("Affiliates"=5, 
                                                             "Allies"=2, 
                                                             "Mergers"=3,
                                                             "Rivals"=1,
                                                             "Splinters"=4)
                              ),
                              
                              sliderInput("range", 
                                          label = "Choose a start and end year:",
                                          min = min(df$year), max = max(df$year), 
                                          value = c(min(df$year), max(df$year)), 
                                          sep = "",
                                          width=360)
                              
                ))),
                
                mainPanel(
                  div(id = "mp",
                      h2("Network Plots"),
                      actionButton('toggleMenu', 'Filter'),
                      tabsetPanel(
                        
                        tabPanel("Spatial",
                                 visNetworkOutput("networkvisfinal",
                                                  width="1200px", 
                                                  height="800px"),
                                 style = "background-color: #eeeeee;"),
                        tabPanel("Hierarchical", 
                                 visNetworkOutput("visnetworktimeline",
                                                  height="500px"),
                                 style = "background-color: #eeeeee;"),
                        tabPanel("Sankey", 
                                 sankeyNetworkOutput("diagram",
                                                     height="500px"),
                                 style = "background-color: #eeeeee;")
                        
                      )
                  )
                )
  ),
  
  # WHERE YOUR FOOTER GOES
  hr(),
  print("Not for publication or distribution.")
  
  # print("Not for publication or distribution. Iris Malone (irismalone@gwu.edu)")
))


