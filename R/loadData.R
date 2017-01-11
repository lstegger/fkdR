#' Loads csv files from facial keypoint recognition contest on kaggle and loads them
#' into data.frames with correct data types. Afterwards they are saved for
#'
#' @return
#' @export
#'
#' @examples
processRawData <- function() {
  print("This may take a while...")
  print("Loading datasets...")
  d.train <<- read.csv(paste0(data.raw.dir, 'faces.train.csv.gz'), stringsAsFactors=F)
  d.test  <<- read.csv(paste0(data.raw.dir, 'faces.test.csv.gz'),  stringsAsFactors=F)

  print("Converting images...")
  # Convert images: chr to int
  d.train$Image  <<- lapply(d.train$Image, function(chrImage) as.integer(unlist(strsplit(chrImage, " "))))
  d.train$Image  <<- lapply(seq_len(nrow(d.train$Image)), function(i) d.train$Image[i,])
  d.test$Image   <<- lapply(d.test$Image,  function(chrImage) as.integer(unlist(strsplit(chrImage, " "))))
  d.test$Image   <<- lapply(seq_len(nrow(d.test$Image)), function(i) d.test$Image[i,])

  coordinate.names <<- gsub("_x", "", names(d.train)[grep("_x", names(d.train))])

  print("Saving preprocessed data...")
  # Save data
  save(d.test,  file=paste0(data.dir, 'faces.test.RData'),  compress = TRUE)
  save(d.train, file=paste0(data.dir, 'faces.train.RData'), compress = TRUE)
}
