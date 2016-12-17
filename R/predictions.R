#' @export
predict <- function(method = 'patch', ...) {

}

#' @export
writeSubmissionFile <- function(predictions, file = "submission.csv") {
  submission <- reshape2::melt(predictions, id.vars="ImageId", variable.name="FeatureName", value.name="Location")
  sub.col.names <- names(example.submission)

  submission <- merge(example.idLookupTable[,-4], submission, all.x=T, sort=F)
  submission <- submission[, sub.col.names]

  write.csv(submission, file=paste0(submission.dir, file), quote=F, row.names=F)
}
