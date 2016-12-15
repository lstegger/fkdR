#' @export
fkdr.predict <- function(method = 'patch', ...) {

}

#' @export
fkdr.writeSubmissionFile <- function(predictions, file = "submission.csv") {
  submission         <- reshape2::melt(predictions, id.vars="ImageId", variable.name="FeatureName", value.name="Location")
  example.submission <- read.csv(paste0(data.dir, 'IdLookupTable.csv'))
  example.submission2 <- read.csv(paste0(data.dir, 'SampleSubmission.csv'))
  sub.col.names <- names(example.submission2)

  submission <- merge(example.submission[,-4], submission, all.x=T, sort=F)
  submission <- submission[, sub.col.names]

  write.csv(submission, file=paste0(data.dir, file), quote=F, row.names=F)
}
