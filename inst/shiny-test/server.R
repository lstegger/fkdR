# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyjs)

shinyServer(function(input, output) {
  # react to img data changes
  observeEvent(input$img, {
    imgData <- input$img
    imgData <- as.integer(unlist(strsplit(imgData, ",")))
    imgData <- imgData[1:9216 * 4 - 3]
    imgData <- matrix(imgData, nrow = 96, ncol = 96)

    # todo: predict keypoints

    output$keypointImg <- renderImage({
      # temp file to save the output.
      outfile <- tempfile(fileext='.png')

      # setup temp img file
      png(outfile, width=480, height=480)
      par(mar = rep(0, 4))

      image(1:96, 1:96, imgData[96:1,96:1], col=gray((0:255)/255), xaxt = "n", yaxt = "n", ann = FALSE, breaks = 0:256)
      # todo: draw keypoints
      dev.off()

      # return the image
      list(src = outfile,
           alt = "Keypoint Image")
    }, deleteFile = TRUE)
  })
})
