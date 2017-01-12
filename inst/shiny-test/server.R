
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyjs)

shinyServer(function(input, output) {
  
  observeEvent(input$img, {
    imgData <- input$img
    imgData <- strsplit(imgData, ",")
    str(imgData)
    
    # todo: 
    # - extract only every 4-th value (input data is in rgba format)
    # - predict keypoints
    # - plot image
  })

})
