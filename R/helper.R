#' @export
init <- function(data.dir = NULL, data.raw.dir = NULL, submission.dir = NULL) {
  # register multicore
  doMC::registerDoMC()
  doParallel::registerDoParallel()

  # set paths
  # todo: remove hotfix
  if(!is.null(data.dir)) {
    data.dir <<- data.dir
    # read submission-relevant files
    example.idLookupTable <- read.csv(paste0(data.dir, 'IdLookupTable.csv'))
    example.submission <- read.csv(paste0(data.dir, 'SampleSubmission.csv'))
  } else {
    temp.dir <- paste0(system.file("data", package="fkdR"), "/")
    # read submission-relevant files
    example.idLookupTable <- read.csv(paste0(temp.dir, 'IdLookupTable.csv'))
    example.submission <- read.csv(paste0(temp.dir, 'SampleSubmission.csv'))
    data.dir <<- temp.dir
  }

  if(!is.null(data.dir)) {
    data.raw.dir  <<- data.raw.dir
  } else {
    data.raw.dir  <<- paste0(system.file("data-raw", package="fkdR"), "/")
  }

  if(!is.null(data.dir)) {
    submission.dir <<- submission.dir
  } else {
    submission.dir <<- paste0(system.file("submission", package="fkdR"), "/")
  }

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
#' plotFacialKeypoints(im.train, 1, d.train)
#' plotFacialKeypoints(im.test, 1)
#' plotFacialKeypoints(im.train.equalized, 1, d.train)
#' plotFacialKeypoints(im.test.equalized, 1)
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

  if(!is.null(keypointPositions)) {
    indices = seq(1, ncol(keypointPositions), 2)
    for (i in indices) {
      points(96-keypointPositions[imgIndex, ][i], 96-keypointPositions[imgIndex, ][i + 1], col = "green", pch = 4)
    }
  }
}

# Create PDF of random 256 training images
#' @export
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

## Make image pixel intensity as bright as a specified mean value
whitenImage <- function(image, meanIntensity) {
  mx = max(image)
  mn = min(image)
  weights = (mx - image) / (mx - mn)
  x = (length(image) * meanIntensity - sum(image)) / sum(weights)
  return(image + weights * x)
}

darkenImage <- function(image, meanIntensity) {
  mx = max(image)
  mn = min(image)
  weights = (image - mn) / (mx - mn)
  x = -(length(image) * meanIntensity - sum(image)) / sum(weights)
  return(image - weights * x)
}

averageImage <- function(image, meanIntensity) {
  m = mean(image)
  if(m < meanIntensity) return(whitenImage(image, meanIntensity))
  else return(darkenImage(image, meanIntensity))
}

vectorToImg <- function(v) {
  matrix(data=rev(v), nrow=96, ncol=96)
}

imgToVector <- function(img) {
  rev(as.vector(img))
}
