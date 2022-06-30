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

# Load data
df <- load_data()

# Pre-process
df <- preprocess(df)

# For the input controls
maps <- c("All", unique(df$map_name))
status_names <- as.character(unique(df$status))
  
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
    
     body{
         background-color:#eeeeee;
         width:2560px;
         height:1440;
     }
      
      #sp
      {
        width:480px;
        height:800px;
        position:absolute;
        z-index:10;
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
      
      
      #range
      {
        font-size:5pt;
      }
      
      .irs-grid-text{
        font-size: 12pt;
        transform: rotate(-90deg) translate(-30px);
      }
      
      #dd_body{
        width:1500px;
        height:800px;
      }
      
      #dd_sp
      {
        width:400px;
        position:absolute;
      }
      
      #dd_mp
     {
       width:600px;
       height:300px;
       left:450px;
       position:absolute;
     }
     
     #about_ct
     {
       width:600px;
       height:600px;
       font-family: Merriweather;
       font-size: 14pt;
       color: #333333;
       text-align: justify;
       margin-left: 200px;
       line-height: 36px;
     }

      
      #db_div{margin-top:70px; margin-bottom:50px;}
      #about_title_div
      {
         width:600px;
         height:auto;
         border-style: none none outset none;
         box-shadow: 0px 7px 16px #888;
      }
      "
    ))
  ),
  
  shinyjs::useShinyjs(),
  
  navbarPage('MMP Prototype 2', selected="vizNM",  
             tabPanel(title='About the App', id='aboutNM',
                      value='aboutNM',
                      div(id="about_ct",
                          tags$div(id='about_title_div',
                          HTML("<h2>&nbsp;Mapping Militants Project</h2>")),
                          tags$br(),
                          tags$div(id="mpp_video", 
                                   HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/5Gj_l0x-SEQ" title="CISAC Who We Are REVISED" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                                   ),
                          tags$br(),
                          tags$p("The MMP research project traces the evolution of militant organizations and the interactions that develop among them over time. Findings are presented in interactive “maps,” which provide both (1) visual representations of how relationships among militant organizations change over time and (2) links to in-depth profiles of individual groups. The project provides uniquely accessible and clear genealogical information about violent extremist organizations that, combined with the detailed group profiles, is an invaluable resource to students, scholars, journalists, policy analysts, and others interested in violent oppositional organizations. The project helps identify patterns in, as well as causes and consequences of, violent extremist group evolution by describing and comparing the genealogy of different families of organizations. Genealogies are presented in interactive diagrams or “maps” that detail how groups form, split, merge, collaborate, compete, shift ideological direction, adopt or renounce violence, grow, shrink, and eventually decline over time.  The MMP research project also provides a database of detailed and documented group profiles. It develops computer software to assemble, organize, and display the profiles and genealogical information that researchers have produced."),
                          tags$br(),
                          tags$p("From 2009 to 2012, MMP was funded by an award from the Social and Behavioral Dimensions of National Security, Conflict, and Cooperation competition, a joint venture between the National Science Foundation and the Department of Defense. From 2012 to 2019 the research was supported by Stanford University, including the Freeman Spogli Institute for International Studies Policy Implementation Lab. In 2019, the project received funding from the", tags$a(href = "https://www.unomaha.edu/ncite/index.php", "National Counterterrorism, Innovation, Technology, and Education Center (NCITE)", .noWS = "outside"), " a U.S. Department of Homeland Security Center of Excellence.  The project relies primarily on research assistance from Stanford undergraduate and graduate students.", '!', .noWS = c("after-begin", "before-end"))
                          )
             ),
             
             tabPanel(title='Download the data', id='downloadNM', value="downloadNM",
                      div(id="dd_body",
                        div(id="dd_sp",
                            
                          selectInput("dd_map_name",
                                      "Select map:",
                                      # For debugging purposes change to maps[1] once finished
                                      selected = maps[1],
                                      choices = maps),
                          
                          sliderInput("dd_range", 
                                      label = "Choose a start and end year:",
                                      min = min(df$year), max = max(df$year), 
                                      value = c(min(df$year), max(df$year)), 
                                      sep = "",
                                      width=360) ,
                          div(id="db_div",
                          downloadButton('downloadData'))
                        ),
                        
                        
                        div(id="dd_mp",
                            dataTableOutput('dataTable')
                          )
                      ) # dd_body
             ), # tabpanel
             
             tabPanel(title='Visualize the Data', id='vizNM', value='vizNM',
                      sidebarLayout(position = "left",
                                    shinyjs::hidden(
                                      div( id="sp", class="sp",
                                           div(id="sp2", 
                                               div(id ="icon_div",
                                                   tags$img(id="closeSp", height = 20, width = 20, 
                                                            src = 'close.png') ),
                                               h2("Options"),
                                               selectInput("map_name",
                                                           "Select map:",
                                                           # For debugging purposes change to maps[1] once finished
                                                           selected = maps[2],
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
                                      div(id = "mp", class="mp",
                                          h2("Network Plots"),
                                          actionButton('toggleMenu', 'Filter'),
                                          tabsetPanel(
                                            
                                            tabPanel("Spatial",
                                                     visNetworkOutput("networkvisfinal",
                                                                      width="2000px", 
                                                                      height="1200px"),
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
                      )
             ),
             tabPanel(title='Profile Links', id='plNM', value='plNM'),
             id='nbp',
  )
  
))


