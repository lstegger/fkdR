#' @export
fkdr.predict <- function(method = 'patch', ...) {

}

fkdr.writeSubmissionFile <- function(predictions) {
  submission         <- melt(predictions, id.vars="ImageId", variable.name="FeatureName", value.name="Location")
  example.submission <- read.csv(paste0(data.dir, 'IdLookupTable.csv'))
  example.submission2 <- read.csv(paste0(data.dir, 'SampleSubmission.csv'))
  sub.col.names <- names(example.submission2)

  submission <- merge(example.submission[,-4], submission, all.x=T, sort=F)
  submission <- submission[, sub.col.names]

  write.csv(submission, file="submission_search.csv", quote=F, row.names=F)
}
