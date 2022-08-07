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
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            textOutput('testTxt'),
            visNetworkOutput('hierarchicalPlot', width="1000px", height="1000px")
        )
    )
))
