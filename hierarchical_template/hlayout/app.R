#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(visNetwork)
source('generate_coordinates.R')

# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$style("", {
      "body{height:1000px; width:1000px;} div{height:100%; width:100%;}"
    }),
    # Application title
    titlePanel("Sample Hierarchical Viz"),



        # Show a plot of the generated distribution
        mainPanel(
           visNetworkOutput("hierarchical", height="1000px", width="1000px")
        )
   
)

# Define server logic required to draw a histogram
# server <- function(input, output) {
# 
#   # title=c('1_2', '1_3', '2_4', '2_5', '3_6', '3_7')
#   e_year <- c(2010, 2012, 2011, 2011, 2013, 2013)
#   n_year <- c(2010, 2011, 2013, 2012, 2012, 2014, 2014, 2014, 2014)
# 
#   # year_r <- rank(year)
#   # maxlen <- 200
#   # edge_len <- log(year_r, 6) * 200 + 50
#   # browser()
#   nodes <- data.frame(id = 1:9,
#                       label=paste("Node",1:9),
#                       title=paste("Node",1:9),
#                       level=n_year
#                       #group=c(2010, 2011, 2013, 2012, 2012, 2014, 2014)
#                       )
#   edges <- data.frame(from = c(1, 1, 2, 2, 3, 3, 8),
#                       to = c(2, 3, 4, 5, 6, 7, 9),
#                       title=c('1_2', '1_3', '2_4', '2_5', '3_6', '3_7', '8_9')
#   )
  # edges$smooth.roundness <- c(0, 0.56, 0, 0, 0, 0, 0)

  # roundness <- c(0)
  # start_year <- min(n_year)
  # for(i in 2:nrow(edges))
  # {
  #   node_y <- n_year[i]
  #   if(node_y - (start_year + 1) != 0)
  #   {
  #     n_nodes <- length(n_year[n_year >= start_year+1 & n_year <= node_y])
  #     roundness <- c(roundness, log(n_nodes) * 3e-1)
  #   }
  # }

  #roundness=c(0, 0.36, 0, 0, 0, 0, 0)

  # browser()
  # output$hierarchical <- renderVisNetwork({
  # visNetwork(nodes, edges) %>%
  #   visHierarchicalLayout(direction = "UD",
  #                         sortMethod = 'directed',
  #                         parentCentralization = T) #%>%
      # visEdges(smooth = list(enabled=T,
      #                        type='curvedCW',
      #                        forceDirection='horizontal'),
      #          arrows=list(to=list(enabled=T)))# %>%
     # visPhysics(hierarchicalRepulsion=list(avoidOverlap=1))
#   })
# }
# load('../nodes.RData')
# load('../edges.RData')
# server <- function(input, output) {
# 
#   # title=c('1_2', '1_3', '2_4', '2_5', '3_6', '3_7')
#   e_year <- c(2010, 2012, 2011, 2011, 2013, 2013)
#   n_year <- c(2010, 2011, 2013, 2012, 2012, 2014, 2014, 2014, 2014)
# 
#   # year_r <- rank(year)
#   # maxlen <- 200
#   # edge_len <- log(year_r, 6) * 200 + 50
#   # browser()
#   nodes <- data.frame(id = 1:9,
#                       label=paste("Node",1:9),
#                       title=paste("Node",1:9)#,
#                       # level=n_year
#                       #group=c(2010, 2011, 2013, 2012, 2012, 2014, 2014)
#   )
#   offset <- 30
#   nodes$x <- c(90, 90, 270, 0, 180, 270, 360, 450, 540)
#   nodes$y <- c(0, 150, 450, 300, 300, 600, 600, 600, 600)
# 
#   edges <- data.frame(from = c(1, 1, 2, 2, 3, 3, 8),
#                       to = c(2, 3, 4, 5, 6, 7, 9),
#                       title=c('1_2', '1_3', '2_4', '2_5', '3_6', '3_7', '8_9')
#   )
#   edges$smooth.roundness <- c(0, 0.56, 0, 0, 0, 0, 0)
  # edges$smooth.roundness <- c(0, 0.56, 0, 0, 0, 0, 0)

  # roundness <- c(0)
  # start_year <- min(n_year)
  # for(i in 2:nrow(edges))
  # {
  #   node_y <- n_year[i]
  #   if(node_y - (start_year + 1) != 0)
  #   {
  #     n_nodes <- length(n_year[n_year >= start_year+1 & n_year <= node_y])
  #     roundness <- c(roundness, log(n_nodes) * 3e-1)
  #   }
  # }

  #roundness=c(0, 0.36, 0, 0, 0, 0, 0)

  # browser()
  # output$hierarchical <- renderVisNetwork({
  #   visNetwork(nodes, edges) %>%
  #     # visHierarchicalLayout(direction = "UD",
  #     #                       sortMethod = 'directed'#,
  #     #                       #parentCentralization = T
  #     #                       ) %>%
  #     visEdges(
  #              smooth = list(enabled=T,
  #                            type='curvedCW',
  #                            forceDirection='horizontal'),
  #              arrows=list(to=list(enabled=T))) %>%
  #     visPhysics(enabled = F)
  #   # visPhysics(hierarchicalRepulsion=list(avoidOverlap=1))
  # })
# }
# load('www/nodes.RData')
# load('www/edges.RData')

# UNCOMMENT THIS-------------------------------------------
server <- function(input, output) {

  output$hierarchical <- renderVisNetwork({
    visNetwork(nodes, edges) %>%
      # visHierarchicalLayout(direction = "UD",
      #                       sortMethod = 'directed'#,
      #                       #parentCentralization = T
      #                       ) %>%
      visEdges(
        # smooth = list(enabled=T,
        #               type='curvedCW',
        #               forceDirection='horizontal'),
        color="black",
        arrows=list(to=list(enabled=T))) %>%
      visPhysics(enabled = F)
    # visPhysics(hierarchicalRepulsion=list(avoidOverlap=1))
  })
}

# server <- function(input, output){
#   nodes <- data.frame(id=c(1,2,3,4,5), 
#                       x=c(90,90,0,90,180),
#                       y=c(0, 150, 300, 300, 300))
#   
#   edges <- data.frame(from=c(1,2,2,2), 
#                       to=c(2,3,4,5))
#   
#   output$hierarchical <- renderVisNetwork({
#     visNetwork(nodes, edges) %>%
#       visEdges(
#         arrows=list(to=list(enabled=T))) %>%
#       visPhysics(enabled = F)
#   })
# }

# Run the application 
shinyApp(ui = ui, server = server)
