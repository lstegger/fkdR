#' @export
predict <- function(method = 'patch', ...) {

}

#' @export
writeSubmissionFile <- function(predictions, filename = "submission.csv") {
  colnames(predictions) = names(d.train[,-31])
  predictions = data.frame(ImageId = 1:nrow(d.test), predictions)

  submission <- reshape2::melt(predictions, id.vars="ImageId", variable.name="FeatureName", value.name="Location")
  head(submission)

  example.submission = read.csv(paste0(data.dir, "IdLookupTable.csv"))
  sub.col.names = names(example.submission)
  example.submission$Location = NULL
  submission = merge(example.submission, submission, all.x=T, sort=F)
  submission = submission[, sub.col.names]
  submission$ImageId = NULL
  submission$FeatureName = NULL
  write.csv(submission, file=paste0(submission.dir, filename), quote=F, row.names=F)
}
