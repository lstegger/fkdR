#' Writes a submission file for uploading the results to kaggle
#'
#' @param predictions The predictions of keypoints of the test dataset without images
#' @param .submission.dir The submission dir (optional; if not set it will be the initialized one)
#' @param filename The filename (optional; default is submission.csv)
#'
#' @export
#'
#' @examples
#' writeSubmissionFile(pred, "~/fkdR/submissions/", "mySubnmission.csv")
writeSubmissionFile <- function(predictions, .submission.dir = submission.dir, filename = "submission.csv") {
  colnames(predictions) = names(d.train[,-"Image"])
  predictions = data.frame(ImageId = 1:nrow(d.test), predictions)

  # reshape predictions
  submission <- reshape2::melt(predictions, id.vars="ImageId", variable.name="FeatureName", value.name="Location")
  head(submission)

  # prepare submission file
  example.submission = read.csv(paste0(data.dir, "IdLookupTable.csv"))
  sub.col.names = names(example.submission)
  example.submission$Location = NULL
  submission = merge(example.submission, submission, all.x=T, sort=F)
  submission = submission[, sub.col.names]
  submission$ImageId = NULL
  submission$FeatureName = NULL

  # check if values are in [0, 96]
  lower0 = which(submission$Location <  0)
  submission[lower0, 2] =  0
  submission[which(submission$Location > 96), 2] = 96

  # write submission file
  write.csv(submission, file=paste0(submission.dir, filename), quote=F, row.names=F)
}
