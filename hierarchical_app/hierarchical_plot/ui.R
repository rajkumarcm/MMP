#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(visNetwork)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Iraq hierarchical plot testing..."),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId="map_name", choices = unique(df$map_name),
                        selected="Global Islamic State", label="Select Map Name:")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
            visNetworkOutput('hierarchicalPlot', width="2000px", height="1600px")
        )
    )
))
