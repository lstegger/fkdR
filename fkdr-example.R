#devtools::install_github("lstegger/fkdR")
library(fkdR)
library(mlr)

# load preprocessed data
data(faces.test,  package="fkdR")
data(faces.train, package="fkdR")
data(faces.test.equalized,  package="fkdR")
data(faces.train.equalized, package="fkdR")

# alternatively preprocess from raw data
# init()
# processRawData()
# prep.equalize()

init()

data.train <- data.frame(foreach(coord = coordinate.names, .combine=cbind) %dopar% {
  coord_x <- paste(coord, "x", sep="_")
  coord_y <- paste(coord, "y", sep="_")
  (round(d.train[,coord_x]) - 1) * 96 + round(d.train[,coord_y])
})

colnames(data.train) <- coordinate.names

data.train$Image <- d.train$Image

d.train.sub <- data.train[1:100,]

## Generate the task
task = makeRegrTask(data = d.train.sub[which(!is.na(d.train.sub[,1])), c("Image", "left_eye_center")], target = "left_eye_center")

rdesc = makeResampleDesc("CV", iters = 5)

## Generate the learner
lrn = makeLearner("regr.patchSearch")

res = resample(learner = lrn, task = task, reampling = rdesc)

ps = makeParamSet(
  makeDiscreteParam("patch_size", values = 3 * 1:5),
  makeDiscreteParam("search_size", values = 2:4)
)
ctrl = makeTuneControlGrid()
tuned = tuneParams(learner = lrn, task = task, resampling = rdesc, par.set = ps, control = ctrl, measures = rmse2d)
opt.grid = as.data.frame(tuned$opt.path)

library(ggplot2)
g = ggplot(tuned$opt.path$env$path, aes(x = patch_size, y = search_size,
                         fill = rmse2d.test.mean, label = round(rmse2d.test.mean, 3)))
g + geom_tile() + geom_text(color = "white")


## Train the learner
mod = train(lrn, task)
mod

pred = predict(mod, newdata = d.train.sub[which(!is.na(d.train.sub[,1])), c("Image", "left_eye_center")])
pred

performance(pred, measures = rmse2d)

