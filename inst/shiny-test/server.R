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
    imgData <- as.matrix(imgData / 255)

    keypointPositions = sess$run(y_conv, feed_dict = dict(x = imgData, keep_prob = 1.0)) * 48 + 48

    output$keypointImg <- renderImage({
      # temp file to save the output.
      outfile <- tempfile(fileext='.png')

      # setup temp img file
      png(outfile, width=480, height=480)
      par(mar = rep(0, 4))

      image(1:96, 1:96, imgData[96:1,96:1], col=gray((0:255)/255), xaxt = "n", yaxt = "n", ann = FALSE, breaks = 0:256)

      # plot keypoints
      indices = seq(1, ncol(keypointPositions[,-c("Image", "ImageId")]), 2)
      for (i in indices) {
        points(96-keypointPositions[imgIndex, ][i], 96-keypointPositions[imgIndex, ][i + 1], col = "green", pch = 4)
      }

      dev.off()

      # return the image
      list(src = outfile,
           alt = "Keypoint Image")
    }, deleteFile = TRUE)
  })
})
