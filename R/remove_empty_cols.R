#' Removes empty columns from a data.frame.
#'
#' Removes all columns from a data.frame that are composed entirely of \code{NA} values.
#'
#' @param x the input data.frame.
#' 
#' @return Returns the data.frame with no empty columns.
#' 
#' @examples
#' # called with magrittr pipe %>% :
#' # library(dplyr)
#' # not run:
#' # x %>% remove_empty_cols
#' 
#' @export
remove_empty_cols <- function(x){
  x[colSums(!is.na(x)) > 0]
}
