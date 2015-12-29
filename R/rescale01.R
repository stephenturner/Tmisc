#' Rescale 0 to 1
#' 
#' Rescales a vector from 0 to 1. Borrowed from the scales package.
#' 
#' @author Stephen Turner
#' @keywords NA
#' 
#' @param x A numeric vector to rescale.
#' 
#' @return The numeric vector scaled from zero to one.
#' 
#' @examples
#' rescale01(runif(10))
#' rescale01(1:11)
#' rescale01(c(1,1))
#' rescale01(1)
#' rescale01(c(1, NA, 2))
#' 
#' @export
rescale01 <- function (x) {
    r <- range(x, na.rm=TRUE, finite=TRUE)
    to <- c(0,1)
    if(length(x)==1 | abs(r[2]-r[1])<1000*.Machine$double.eps) {
        return(ifelse(is.na(x), NA, mean(to)))
    }
    (x - r[1])/diff(r) * diff(to) + to[1]
}
