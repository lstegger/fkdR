#devtools::install_github("lstegger/fkdR")
library(fkdR)
library(mlr)
library(doParallel)

doParallel::registerDoParallel()

# initialize fkdR package (load data, initialize directories, read coordinate names)
init()

# alternatively preprocess from raw data
# init()
# processRawData()
# prep.equalize()

# transform x and y coordinates to a single value
data.train <- data.frame(foreach(coord = coordinate.names, .combine=cbind) %dopar% {
  coord_x <- paste(coord, "x", sep="_")
  coord_y <- paste(coord, "y", sep="_")
  (round(d.train[,coord_x]) - 1) * 96 + round(d.train[,coord_y])
})
# set coordinate names
colnames(data.train) <- coordinate.names

# copy images to training data set
data.train$Image <- im.train.equalized

# prepare test data set
data.test <- d.test
data.test$Image <- im.test.equalized
data.test[,coordinate.names] <- NA

# learn models and predict coordinates for each key point
results <- foreach(coord = coordinate.names[3:5]) %do% {
  print(coord)
  # generate the task
  task = makeRegrTask(data = data.train[which(!is.na(data.train[,coord])),
                                         c("Image", coord)],
                      target = coord)

  # use cross-validation with 5 iterations
  rdesc = makeResampleDesc("CV", iters = 2)

  # generate the learner
  lrn = makeLearner("regr.patchSearch")

  # prepare tuning
  ps = makeParamSet(
    makeDiscreteParam("patch_size", values = 3 * 2:5),
    makeDiscreteParam("search_size", values = 2:4)
  )
  ctrl = makeTuneControlGrid()
  tuned = tuneParams(learner = lrn, task = task, resampling = rdesc, par.set = ps, control = ctrl, measures = rmse2d())

  # plot tuning results using root mean squared error
  library(ggplot2)
  g = ggplot(tuned$opt.path$env$path, aes(x = patch_size, y = search_size,
                           fill = rmse2d.test.mean, label = round(rmse2d.test.mean, 3)))
  g = g + geom_tile() + geom_text(color = "white")
  g

  # determine best params
  params.tuned <- tuned$opt.path$env$path[which.min(tuned$opt.path$env$path$rmse2d.test.mean),]

  # generate the learner with tuned params
  lrn = makeLearner("regr.patchSearch",
                    patch_size = as.integer(params.tuned$patch_size),
                    search_size = as.integer(params.tuned$search_size))

  mod = train(learner = lrn, task = task)

  pred = predict(mod, newdata = data.test)

  list(model = mod, prediction = pred, params = params.tuned, tuneplot = g)
}

