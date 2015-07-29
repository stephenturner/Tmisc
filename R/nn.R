#' Get names and class of all columns in a data frame
#' 
#' Get names and class of all columns in a data frame in a friendly format.
#' 
#' @author Stephen Turner
#' @keywords NA
#' 
#' @param df A data.frame.
#' 
#' @return A data.frame with index and class.
#' 
#' @export
#' 
#' @examples
#' nn(iris)

nn <- function(df) data.frame(var=names(df), 
                              index=1:ncol(df), 
                              class=sapply(df, class), 
                              row.names=NULL)
nn(iris)
