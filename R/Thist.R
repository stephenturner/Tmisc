#' Histograms with overlays
#' 
#' Plot a histogram with either a normal distribution or density curve overlay.
#' 
#' @author Stephen Turner
#' @keywords NA
#' 
#' @param x A numeric vector.
#' @param overlay Either "normal" (default) or "density" indicating whether a normal distribution or density curve should be plotted on top of the histogram.
#' @param col Color of the histogram bars.
#' @param ... Other arguments to be passed to \code{hist()}.
#' 
#' @importFrom stats density dnorm na.omit
#' @importFrom graphics hist rug lines
#' 
#' @examples
#' set.seed(42)
#' x <- rnorm(1000, mean=5, sd=2)
#' Thist(x)
#' Thist(x, overlay="density")
#' Thist(x^2)
#' Thist(x^2, overlay="density", breaks=50, col="lightblue2")
#' 
#' @export
Thist <- function(x, overlay="normal", col="gray80", ...) {
    stopifnot(is.numeric(x) & is.vector(x))
    stopifnot(overlay=="normal" | overlay=="density")
    if (any(is.na(x))) {
        warning(paste(sum(is.na(x)), "missing values"))
        x <- na.omit(x)
    }
    h <- hist(x, plot=FALSE, ...)
    if (overlay=="normal") {
        xfit <- seq(min(x), max(x), length=40)
        yfit <- dnorm(xfit, mean=mean(x), sd=sd(x))
        yfit <- yfit*diff(h$mids[1:2])*length(x)
        hist(x, ylim=c(0,max(yfit, h$counts)), col=col, ...)
        lines(xfit,yfit, col="blue", lwd=2)
    } else if (overlay=="density") {
        hist(x, probability=TRUE, ylim=c(0, max(density(x)$y, h$density)), col=col, ...)
        lines(density(x), col = "red", lwd=2)
    }
    rug(x)
}