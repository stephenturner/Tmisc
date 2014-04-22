#' Un-rowname
#' 
#' Strip rownames from an object (stolen from plyr).
#' 
#' @param x data frame
#'   
#' @return Character vector of the gene symbol with the probe ID
#'   
#' @keywords keywords
#'   
#' @export

unrowname <- function(x) {
    rownames(x) <- NULL
    x
}
