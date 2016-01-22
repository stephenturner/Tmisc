#' Rescale a vector
#' 
#' Rescale a vector to have specified min and max. Borrowed from the scales package.
#' 
#' @author Stephen Turner
#' @keywords NA
#' 
#' @param x A numeric vector to rescale.
#' 
#' @return The numeric vector scaled from zero to one.
#' 
#' @examples
#' rescale(runif(10))
#' rescale(1:11)
#' rescale(c(1,1))
#' rescale(1)
#' rescale(c(1, NA, 2))
#' 
#' @export
rescale <- function (x, to=c(0,1)) {
    r <- range(x, na.rm=TRUE, finite=TRUE)
    if(length(x)==1 | abs(r[2]-r[1])<1000*.Machine$double.eps) {
        return(ifelse(is.na(x), NA, mean(to)))
    }
    (x - r[1])/diff(r) * diff(to) + to[1]
}
