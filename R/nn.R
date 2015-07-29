#' Get class of all data frame columns
#' 
#' Get class of all data frame columns in a friendly format
#' 
#' @author Stephen Turner
#' @keywords keywords
#' 
#' @param df A data.frame object
#' 
#' @return A data.frame with index and class
#' 
#' @export
#' 
#' @examples
#' dfclass(iris)

dfclass <- function(df) data.frame(index=1:ncol(df), class=sapply(df, class))
