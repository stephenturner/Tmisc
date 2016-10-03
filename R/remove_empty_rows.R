#' Removes empty rows from a data.frame.
#'
#' Removes all rows from a data.frame that are composed entirely of \code{NA} values.
#'
#' @param x the input data.frame.
#' 
#' @return Returns the data.frame with no empty rows.
#' 
#' @examples
#' # called with magrittr pipe %>% :
#' # library(dplyr)
#' # not run:
#' # x %>% remove_empty_rows
#' 
#' @export
remove_empty_rows <- function(x){
  x[rowSums(is.na(x)) != ncol(x), ]
}
