#' Head-tail
#' 
#' Prints the \code{head} and \code{tail} of a data.frame or matrix.
#' 
#' @author Stephen Turner
#' @keywords keywords
#' 
#' @param d The data frame or matrix you want to head-tail
#' @param ... additional arguments passed to head and tail
#' 
#' @return The first and last few lines of a data frame or matrix.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' ht(iris)
#' }

ht <- function(d, ...) {
    if(class(d)=="matrix"|class(d)=="data.frame") {
        rbind(head(d, ...),tail(d, ...))
    }
}
