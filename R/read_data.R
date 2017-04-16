#' read_data
#'
#' @param train \code{character} path to training dataset (matrix) - must be .csv
#' @param labels \code{character} path to training dataset (labels) - must be .txt
#' @param features \code{character} path to training dataset (features) - must be .txt
#'
#' @return df \code{data.frame} returns a data.frame containing training data with labels and features.
#' @export
#'
#' @examples
#' \dontrun {
#' dir("out/data_import") #show files in a directory
#' train <- "out/data_import/contract_binarytrain_data.csv"
#' labels <- "out/data_import/contract_binarytrain_labels.txt"
#' features <- "out/data_import/contract_binarytrain_vocabulary.txt"
#' df <- read_data(train, labels, features)
#' }


read_data <- function(train, labels, features=NULL) {
  train <- read.csv2(train, header=F, stringsAsFactors = F)
  labels <- read.table(labels, header=F, stringsAsFactors = F)[,1]
  df <- apply(train, 2, as.numeric) %>% as.data.frame
  if (!is.null(features)) {
    features <- read.table(features, header=F, stringsAsFactors = F)[,1]
    names(df) <- features
  }
  df[["contract_label"]] <- labels
  df
}

