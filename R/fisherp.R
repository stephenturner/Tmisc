#' Fisher's method to combine p-values.
#' 
#' Uses Fisher's method to combine p-values from different tests.
#' 
#' @author Stephen Turner
#' @keywords keywords
#'   
#' @param x A vector of p-values between 0 and 1.
#'   
#' @return A combined p-value.
#' 
#' @importFrom stats na.omit pchisq
#'   
#' @examples
#' fisherp(c(.042, .02, .001, 0.01, .89))
#' 
#' @export
fisherp <- function(x) {
    if (any(is.na(x))) {
        warning("Some p-values missing; removing these.")
        x <- na.omit(x)
    }
    if (any(x<=0 | x>1)) stop("P-values must be >0 and <=1.")
    if (length(x)<2) stop("Must have at least two valid p-values.")
    df <- 2*length(x)
    fisherp <- pchisq( -2*sum(log(x)), df, lower.tail=FALSE)
    return(fisherp)
}