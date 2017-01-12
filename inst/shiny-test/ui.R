
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
library(shiny)

shinyUI(fluidPage(
  useShinyjs(),
  tags$head(tags$link(rel="stylesheet", href="style.css")),
  tags$body(
    titlePanel("Facial Keypoint Detection"),
    
    # video (hidden by css)
    tags$div(id="videowrapper", tags$video(autoplay="autoplay", id="video", width="128", height="96")),
    
    # canvas
    tags$canvas(id="canvas", width="96", height="96"),
    tags$canvas(id="canvasGray", width="96", height="96"),
    
    # plot with keypoints
    textOutput("test"),
    
    tags$script(src="script.js")
  )
))
