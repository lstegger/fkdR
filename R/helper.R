#' Initializes the fkdR package by loading all relevant data and initializing relevant paths.
#'
#' @param .data.dir Path to preprocessed data (optional)
#' @param .data.raw.dir Path to csv raw data (optional)
#' @param .submission.dir Path where submission files should be stored (optional)
#'
#' @export
#'
#' @examples
#' init(.submission.dir = "~/fkdR/submissions")
init <- function(.data.dir = NULL, .data.raw.dir = NULL, .submission.dir = NULL) {
  # register multicore
  doMC::registerDoMC()
  doParallel::registerDoParallel()

  # set directories
  data.dir <<-       ifelse(is.null(.data.dir),
                            paste0(system.file("data", package="fkdR"), "/"),
                            .data.dir)

  data.raw.dir <<-   ifelse(is.null(.data.raw.dir),
                            paste0(system.file("data-raw", package="fkdR"), "/"),
                            .data.raw.dir)

  submission.dir <<- ifelse(is.null(.submission.dir),
                            paste0(system.file("submission", package="fkdR"), "/"),
                            .submission.dir)

  # read kaggle data
  data(faces.test)
  data(faces.test.equalized)
  data(faces.train)
  data(faces.train.equalized)

  # list the coordinates we have to predict
  coordinate.names <<- gsub("_x", "", names(d.train)[grep("_x", names(d.train))])
}

#' @title Plot some images incl. keypoints
#'
#' @param imgSet The image set as Matrix with 9216 columns
#' @param imgIndex The index of the image that should be plotted
#' @param keypointPositions The keypoint positions as data.frame
#' @param meanIntensity Value between 0 and 255 determining the average intensity the printed image should have
#' @param histEqualize If set to true the image will be histogram equalized before printing
#' @param order Determines wether the image should be normalized or equalized first if both functions should be applied
#' @examples
#' plotFacialKeypoints(d.train$Image, 1, d.train)
#' plotFacialKeypoints(d.test$Image, 1)
#' plotFacialKeypoints(d.train$Image.equalized, 1, d.train)
#' plotFacialKeypoints(d.test$Image.equalized, 1)
#'
#' @export
plotFacialKeypoints <- function(imgSet, imgIndex, keypointPositions = NULL, meanIntensity = NULL, histEqualize = FALSE, order = c("normalize", "equalize")) {
  img = vectorToImg(imgSet[imgIndex, ])

  if(!is.null(meanIntensity) && histEqualize) {
    if(all(order == c("normalize", "equalize"))) {
      img = averageImage(img, meanIntensity)
      img = histeq(img)
    } else if(all(order == c("equalize", "normalize"))) {
      img = histeq(img)
      img = averageImage(img, meanIntensity)
    } else {
      stop("wrong order parameters")
    }
  } else {
    if(!is.null(meanIntensity)) {
      img = averageImage(img, meanIntensity)
    }

    if(histEqualize) {
      img = histeq(img)
    }
  }

  par(mar = rep(0, 4))
  image(1:96, 1:96, img, col=gray((0:255)/255), xaxt = "n", yaxt = "n", ann = FALSE, breaks = 0:256)

  # plot keypoint positions
  if(!is.null(keypointPositions)) {
    indices = seq(1, ncol(keypointPositions[,-c("Image", "ImageId")]), 2)
    for (i in indices) {
      points(96-keypointPositions[imgIndex, ][i], 96-keypointPositions[imgIndex, ][i + 1], col = "green", pch = 4)
    }
  }
}

#' Create PDF of random 256 training images
#'
#' @param imgSet The image set as Matrix with 9216 columns
#' @param keypointPositions The keypoint positions as data.frame
#' @param name Filename of the pdf file
#' @param meanIntensity Value between 0 and 255 determining the average intensity the printed image should have
#' @param histEqualize If set to true the image will be histogram equalized before printing
#' @param order Determines wether the image should be normalized or equalized first if both functions should be applied
#'
#' @export
#'
#' @examples
#' imagesToPdf(d.train$Image, d.train)
#' imagesToPdf(d.test$Image, histEqualize = TRUE)
imagesToPdf <- function(imgSet, keypointPositions, name = "images.pdf", meanIntensity = NULL, histEqualize = FALSE, order = c("normalize", "equalize")) {
  dev.new()
  dev.off()
  par(no.readonly = TRUE)
  pdf(file = paste0(data.dir, name), paper = 'a4')
  par(mfrow = c(4, 4))
  for (index in seq_len(nrow(imgSet))) {
    plotFacialKeypoints(imgSet, index, keypointPositions, meanIntensity, histEqualize, order)
  }
  dev.off()
}

#' Helper method for averageImage()
#' Calculates the new image if it has to be whitened
#'
#' @param image the image
#' @param meanIntensity the desired mean intensity
#'
#' @return
#'
#' @examples
whitenImage <- function(image, meanIntensity) {
  mx = max(image)
  mn = min(image)
  weights = (mx - image) / (mx - mn)
  x = (length(image) * meanIntensity - sum(image)) / sum(weights)
  return(image + weights * x)
}

#' Helper method for averageImage()
#' Calculates the new image if it has to be darkened
#'
#' @param image the image
#' @param meanIntensity the desired mean intensity
#'
#' @return
darkenImage <- function(image, meanIntensity) {
  mx = max(image)
  mn = min(image)
  weights = (image - mn) / (mx - mn)
  x = -(length(image) * meanIntensity - sum(image)) / sum(weights)
  return(image - weights * x)
}

#' Make image pixel intensity as bright as a specified mean value
#'
#' @param image
#' @param meanIntensity
#'
#' @return
#'
#' @examples
#' averageImage(d.train$Image[1], 128)
averageImage <- function(image, meanIntensity) {
  m = mean(image)
  # if average intensity of current picture is lover than the
  if(m < meanIntensity) return(whitenImage(image, meanIntensity))
  else return(darkenImage(image, meanIntensity))
}

#' The kaggle dataset contains images as vectors. These vectors are reversed (last pixel first)
#' and contain the image columnwise. This function converts a kaggle formatted vector to an image.
#'
#' @param v the vector containing the image
#'
#' @return the image as matrix
#' @export
vectorToImg <- function(v) {
  matrix(data=rev(v), nrow=96, ncol=96)
}

#' The kaggle dataset contains images as vectors. These vectors are reversed (last pixel first)
#' and contain the image columnwise. This function converts an image back to the kaggle format.
#'
#' @param img the image as matrix
#'
#' @return the image as vector
#' @export
imgToVector <- function(img) {
  rev(as.vector(img))
}
