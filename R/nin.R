#' x not in y
#' 
#' Returns a logical vector of elements of x that are not in y.
#' 
#' @author Stephen Turner
#' @keywords NA
#' 
#' @param x a vector (numeric, character, factor)
#' @param table a vector (numeric, character, factor), matching the mode of x
#' 
#' @return A logical vector with length equal to \code{x} of things in \code{x} that aren't in \code{y}.
#' 
#' @examples
#' 1:10 %nin% seq(from=2, to=10, by=2)
#' c("a", "b", "c") %nin% c("a", "b")
#' letters[letters %nin% unlist(strsplit("pack my box with five dozen liquor jugs", ""))]
#' 
#' @export
`%nin%` <- function (x, table) match(x, table, nomatch = 0) == 0
