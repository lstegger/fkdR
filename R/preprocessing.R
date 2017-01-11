#' Apply histogram equalization on all images in order to improve contrast and save
#' preprocessed data in data directory.
#'
#' @export
prep.equalize <- function() {
  print("Applying histogram equalization on test images...")
  im.test.equalized <<- do.call(rbind, lapply(1:nrow(im.test), function(i) {
    if ((i %% 100)==0) { cat(sprintf("%d/%d\n", i, nrow(im.test))) }
    IM::histeq(im.test[i,])
  }))

  print("Applying histogram equalization on train images...")
  im.train.equalized <<- do.call(rbind, lapply(1:nrow(im.train), function(i) {
    if ((i %% 100)==0) { cat(sprintf("%d/%d\n", i, nrow(im.train))) }
    IM::histeq(im.train[i,])
  }))

  d.test.equalized <<- round(im.test.equalized)
  im.train.equalized <<- round(im.train.equalized)

  storage.mode(im.test.equalized) <<- "integer"
  storage.mode(im.train.equalized) <<- "integer"

  print("Saving preprocessed data...")
  save(im.test.equalized,  file=paste0(data.dir, 'faces.test.equalized.RData'),  compress = TRUE)
  save(im.train.equalized, file=paste0(data.dir, 'faces.train.equalized.RData'), compress = TRUE)
}

