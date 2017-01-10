
#' Title
#'
#' @param f
#' @param d.tr
#' @param patch_size
#' @param search_size
#'
#' @return
#' @export
#'
#' @examples
patchSearch.train <- function(f, d.tr, patch_size = 10, search_size = 2) {
  str(d.tr)
  coord = all.vars(f)[1]

  cat(sprintf("computing mean patch for %s\n", coord))
  coord_x <- paste(coord, "x", sep="_")
  coord_y <- paste(coord, "y", sep="_")

  # compute average patch
  patches <- foreach (i = 1:nrow(d.tr), .combine=rbind) %do% {
    if ((i %% 100)==0) { cat(sprintf("Extracting %s patch from training image %d/%d\n", coord, i, nrow(d.tr))) }
    im  <- matrix(data = d.tr[i,"Image"], nrow=96, ncol=96)
    # image(1:96, 1:96, im, col=gray((0:255)/255), xaxt = "n", yaxt = "n", ann = FALSE, breaks = 0:256)
    xy  <- d.tr[i, coord]
    x   <- xy %/% 96 + 1
    y   <- xy %% 96
    x1  <- (x-patch_size)
    x2  <- (x+patch_size)
    y1  <- (y-patch_size)
    y2  <- (y+patch_size)
    if ( (!is.na(x)) && (!is.na(y)) && (x1>=1) && (x2<=96) && (y1>=1) && (y2<=96) )
    {
      as.vector(im[x1:x2, y1:y2])
    }
    else
    {
      NULL
    }
  }

  # return mean patch
  mean.patch = matrix(data = colMeans(patches), nrow=2*patch_size+1, ncol=2*patch_size+1)
  image(1:(2*patch_size+1), 1:(2*patch_size+1), mean.patch, col=gray((0:255)/255), breaks = 0:256)
  mean.patch
}

#' Title
#'
#' @param model
#' @param f
#' @param d.te
#' @param patch_size
#' @param search_size
#'
#' @return
#' @export
#'
#' @examples
patchSearch.predict <- function(model, f, d.te, patch_size = 10, search_size = 2)  {
  str(d.te)
  mean.patch = model$learner.model

  # the coordinates we want to predict
  coord = all.vars(f)[1]
  coord_x <- paste(coord, "x", sep="_")
  coord_y <- paste(coord, "y", sep="_")

  # the average of them in the training set (our starting point)
  mean_x  <- mean(d.train[, coord_x], na.rm=T)
  mean_y  <- mean(d.train[, coord_y], na.rm=T)

  # str(mean_x)
  # str(mean_y)
  # str(patch_size)
  # str(search_size)

  # search space: 'search_size' pixels centered on the average coordinates
  x1 <- as.integer(mean_x)-search_size
  x2 <- as.integer(mean_x)+search_size
  y1 <- as.integer(mean_y)-search_size
  y2 <- as.integer(mean_y)+search_size

  # str(x1)
  # str(y1)
  # str(x2)
  # str(y2)

  # ensure we only consider patches completely inside the image
  x1 <- ifelse(x1-patch_size<1,  patch_size+1,  x1)
  y1 <- ifelse(y1-patch_size<1,  patch_size+1,  y1)
  x2 <- ifelse(x2+patch_size>96, 96-patch_size, x2)
  y2 <- ifelse(y2+patch_size>96, 96-patch_size, y2)
#
#   str(x1)
#   str(y1)
#   str(x2)
#   str(y2)

  # build a list of all positions to be tested
  params <- expand.grid(x = x1:x2, y = y1:y2)

  # for each image...
  r <- foreach(i = 1:nrow(d.te), .combine=rbind) %do% {
    im <- matrix(data = d.te[i, "Image"], nrow=96, ncol=96)
    if ((i %% 100)==0) { cat(sprintf("Predicting %s for test image %d/%d\n", coord, i, nrow(d.te))) }

    # ... compute a score for each position ...
    r  <- foreach(j = 1:nrow(params), .combine=rbind) %do% {
      x     <- params$x[j]
      y     <- params$y[j]
      p     <- im[(x-patch_size):(x+patch_size), (y-patch_size):(y+patch_size)]
      score <- cor(as.vector(p), as.vector(mean.patch))
      score <- ifelse(is.na(score), 0, score)
      data.frame(x, y, score)
    }

    # ... and return the best
    best <- r[which.max(r$score), c("x", "y")]
    best
  }

  result <- (round(r[1]) - 1) * 96 + round(r[2])
  str(result[1]$x)
  result[1]$x
}



#' Title
#'
#' @return
#' @export
#'
#' @examples
makeRLearner.regr.patchSearch = function() {
  makeRLearnerRegr(
    cl = "regr.patchSearch",
    package = "fkdR",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "patch_size", default = 10, lower = 0, tunable = TRUE),
      makeNumericLearnerParam(id = "search_size", default = 2, lower = 1, tunable = TRUE)
    ),
    properties = c("numerics", "factors"),
    name = "Patch Search Learner for Keypoint Detection in Images",
    short.name = "patchSearch",
    note = ""
  )
}

#' @export
trainLearner.regr.patchSearch = function(.learner, .task, .subset, ...) {
  str(.subset)
  patchSearch.train(f = getTaskFormula(.task),
                    d.tr = getTaskData(.task, .subset),
                    ...)
}

#' Title
#'
#' @param .learner
#' @param .model
#' @param .newdata
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
predictLearner.regr.patchSearch = function(.learner, .model, .newdata, ...) {
  str(.newdata)
  .patch_size = .model$learner$par.vals$patch_size
  .search_size = .model$learner$par.vals$search_size

  .patch_size = ifelse(is.null(.patch_size), 10, .patch_size)
  .search_size = ifelse(is.null(.search_size), 2, .search_size)

  patchSearch.predict(.model,
                      f = getTaskFormula(.model$task.desc),
                      d.te = .newdata,
                      patch_size = .patch_size,
                      search_size = .search_size,
                      ...)
}

## Define a function that calculates the misclassification rate
#' Title
#'
#' @param task
#' @param model
#' @param pred
#' @param feats
#' @param extra.args
#'
#' @return
#' @export
#'
#' @examples
rmse2d.fun = function(task, model, pred, feats, extra.args) {
  response = data.frame(
    x = getPredictionResponse(pred) %% 96 + 1,
    y = getPredictionResponse(pred) %/% 96
  )
  truth = data.frame(
    x = getPredictionTruth(pred) %% 96 + 1,
    y = getPredictionTruth(pred) %/% 96
  )

  rmse = sqrt((response$x-truth$x)^2 + (response$y-truth$y)^2)
  str(rmse)
  mean(rmse)
}

## Generate the Measure object
#' Title
#'
#' @return
#' @export
#'
#' @examples
rmse2d = function() {
  mlr::makeMeasure(
    id = "rmse2d", name = "Root Mean Squared Error for 2-dimensional data",
    properties = c("regr", "req.pred", "req.truth"),
    minimize = TRUE, best = 0, worst = Inf,
    fun = rmse2d.fun
  )
}
