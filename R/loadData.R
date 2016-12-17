### TEMP
#data.dir = '/Users/henry/Dropbox/Universität/Information Systems M.Sc./_Applied Machine Learning WT 2016/data/'

#' @export
#'
#' @examples
processRawData <- function() {
  print("This may take a while...")
  print("Loading datasets...")
  d.train <<- read.csv(paste0(data.raw.dir, 'faces.train.csv.gz'), stringsAsFactors=F)
  d.test  <<- read.csv(paste0(data.raw.dir, 'faces.test.csv.gz'),  stringsAsFactors=F)

  # Put images in separate variable
  # im.train      <<- d.train$Image
  # d.train$Image <<- NULL
  # im.test       <<- d.test$Image
  # d.test$Image  <<- NULL

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

  df <- data.frame(foreach(coord = coordinate.names) %dopar% {
    cat(sprintf("computing mean patch for %s\n", coord))
    coord_x <- paste(coord, "x", sep="_")
    coord_y <- paste(coord, "y", sep="_")

    # compute average patch
    as.vector(foreach (i = 1:nrow(d.train), .combine=rbind) %do% {
      x   <- round(d.train[i, coord_x])
      y   <- round(d.train[i, coord_y])
      96 * (y - 1) + x
    })
  })

  colnames(df) <- coordinate.names
  d.train <<- cbind(d.train, df)
}

