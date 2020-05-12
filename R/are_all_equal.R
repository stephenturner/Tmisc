#' Are all equal? 
#' 
#' Are all the elements of a numeric vector (approximately) equal?
#' 
#' @author Stephen Turner
#' @keywords keywords
#'   
#' @param x A numeric vector.
#' @param na.rm Remove missing values (FALSE by default; NAs in x will return NA).
#' 
#'   
#' @return Logical, whether all elements of a numeric vector are equal.
#' 
#' @examples
#' are_all_equal(c(5,5,5))
#' are_all_equal(c(5,5,5,6))
#' are_all_equal(c(5,5,5,NA,6))
#' are_all_equal(c(5,5,5,NA,6), na.rm=TRUE)
#' 5==5.000000001
#' identical(5, 5.000000001)
#' are_all_equal(c(5L, 5, 5.000000001))
#' 
#' @export
are_all_equal <- function(x, na.rm=FALSE) {
    stopifnot(is.numeric(x))
    if(na.rm) x <- x[!is.na(x)]
    abs(max(x)-min(x)) < .Machine$double.eps^.5
}