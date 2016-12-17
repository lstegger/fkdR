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

d.train.sub <- d.train[1:100,]

## Generate the task
task = makeRegrTask(data = d.train[which(!is.na(d.train[,1])), c(31, 32)], target = "left_eye_center")

## Generate the learner
lrn = makeLearner("regr.patchSearch")

## Train the learner
mod = train(lrn, task)
mod
