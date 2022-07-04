#to run shiny app
#setwd("/users/irismalone/Dropbox/NCITE/MMP/prototypev2/")
library(shiny)
# runApp()

# library(profvis)
# profvis::profvis({ runApp(getwd()) }  
#         , prof_output = '/profiling_output')

# vwr = dialogViewer('./', width = 1600, height = 1200)

# runGadget(shinyAppDir(appDir = './'), viewer = vwr)
runGadget(shinyAppDir(appDir = './'), viewer=browserViewer(browser=getOption('browser')))
