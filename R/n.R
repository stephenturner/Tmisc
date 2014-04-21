#' Returns names(df) in single column, numbered matrix format.
#' 
#' Given data.frame df, return the `names` of that data.frame
#' in a "long" one-column numbered format.
#' 
#' @param df A data.frame object \{code df}
#' 
#' @return output A single column numbered list of data.frame names
#' 
#' @keywords keywords
#' 
#' @export
#' 
#' @examples
#' n(iris)

n <- function(df) matrix(names(df)) 