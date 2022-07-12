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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Sample Hierarchical Viz"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           visNetworkOutput("hierarchical")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  nodes <- data.frame(id = 1:7, label=paste("Node",1:7), title=paste("Node",1:7))
  edges <- data.frame(from = c(1, 1, 2, 2, 3, 3), to = c(2, 3, 4, 5, 6, 7),
                      title=c('1_2', '1_3', '2_4', '2_5', '3_6', '3_7'))
  
  output$hierarchical <- renderVisNetwork({
  visNetwork(nodes, edges) %>%
    visHierarchicalLayout(direction = "UD", sortMethod = 'directed') %>%
      visEdges(smooth = list(enabled=T),
               arrows=list(to=list(enabled=T))) %>%
      visPhysics(hierarchicalRepulsion=list(avoidOverlap=1))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
