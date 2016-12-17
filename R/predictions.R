#' @export
fkdr.predict <- function(method = 'patch', ...) {

}

#' @export
fkdr.writeSubmissionFile <- function(predictions, filename = "submission.csv") {
  submission <- reshape2::melt(predictions, id.vars="ImageId", variable.name="FeatureName", value.name="Location")
  head(submission)

  example.submission = read.csv(paste0(data.dir, "IdLookupTable.csv"))
  sub.col.names = names(example.submission)
  example.submission$Location = NULL
  submission = merge(example.submission, submission, all.x=T, sort=F)
  submission = submission[, sub.col.names]
  submission$ImageId = NULL
  submission$FeatureName = NULL
  write.csv(submission, file=paste0(data.dir, filename), quote=F, row.names=F)
}
