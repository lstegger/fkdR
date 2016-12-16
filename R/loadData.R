### TEMP
data.dir = '/Users/henry/Dropbox/Universität/Information Systems M.Sc./_Applied Machine Learning WT 2016/data/'


#' @export
loadData <- function() {
  file = paste0(data.dir, 'preprocessedData.Rd')

  if(file.exists(file)) {
    print("Found preprocessed data. Loading data...")
    load(file, envir = .GlobalEnv)
  } else {
    print("No preprocessed data found. Loading datasets...")
    d.train <<- read.csv(paste0(data.dir, 'training.csv'), stringsAsFactors=F)
    d.test <<- read.csv(paste0(data.dir, 'test.csv'), stringsAsFactors=F)

    # Put images in separate variable
    im.train      <<- d.train$Image
    d.train$Image <<- NULL
    im.test       <<- d.test$Image
    d.test$Image  <<- NULL

    print("Converting images...")
    # Convert images: chr to int
    im.train <<- do.call(rbind, lapply(im.train, function(chrImage) as.integer(unlist(strsplit(chrImage, " ")))))
    im.test <<- do.call(rbind, lapply(im.test, function(chrImage) as.integer(unlist(strsplit(chrImage, " ")))))

    print("Saving preprocessed data...")
    # Save data
    save(d.train, im.train, d.test, im.test, file=paste0(data.dir, 'preprocessedData.Rd', compress = TRUE))
  }
}

