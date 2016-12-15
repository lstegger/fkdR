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

  im.test.equalized <<- round(im.test.equalized)
  im.train.equalized <<- round(im.train.equalized)

  storage.mode(im.test.equalized) <<- "integer"
  storage.mode(im.train.equalized) <<- "integer"

  print("Saving preprocessed data...")
  save(d.train, im.train, d.test, im.test, im.train.equalized, im.test.equalized, file=paste0(data.dir, 'preprocessedData.Rd'), compress = TRUE)
}

# ideas: mirrored copies as further inputs
