#' Numbered list of column names
#' 
#' Given data.frame df, return the `names` of that data.frame
#' in a "long" one-column numbered format.
#' 
#' @author Stephen Turner
#' @keywords keywords
#' 
#' @param df A data.frame object
#' 
#' @return A single column numbered list of data.frame names
#' 
#' @export
#' 
#' @examples
#' n(iris)

n <- function(df) {
    matrix(names(df))
}