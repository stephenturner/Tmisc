#' Better scatterplot matrices.
#' 
#' A matrix of scatter plots with rugged histograms, correlations, and significance stars. Much of the functionality borrowed from \code{PerformanceAnalytics::chart.Correlation()}.
#' 
#' @author Stephen Turner
#' @keywords NA
#' 
#' @param x A numeric matrix or data.frame.
#' @param histogram Overlay a histogram on the diagonals?
#' @param gap distance between subplots, in margin lines.
#' @param ... arguments to be passed to or from other methods.
#' 
#' @importFrom stats cor cor.test density dnorm sd symnum
#' @importFrom graphics hist lines pairs panel.smooth par rug strwidth text
#' 
#' @examples
#' Tpairs(iris[-5])
#' Tpairs(iris[-5], pch=21, bg=Tcols[factor(iris$Species)])
#' Tpairs(iris[-5], pch=21, bg=gghues(3)[factor(iris$Species)], gap=1)
#' 
#' @export
Tpairs <- function (x, histogram = TRUE, gap=0, ...) 
{
    x=as.matrix(x)
    if (mode(x)!="numeric") stop("Must pass in only numeric values")
    panel.cor <- function(x, y, digits = 2, prefix = "", use = "pairwise.complete.obs", cex.cor, ...) {
        usr <- par("usr")
        on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        r <- abs(cor(x, y, use = use))
        txt <- format(c(r, 0.123456789), digits = digits)[1]
        txt <- paste(prefix, txt, sep = "")
        if (missing(cex.cor)) cex <- 0.8/strwidth(txt)
        test <- cor.test(x, y)
        Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " "))
        text(0.5, 0.5, txt, cex = cex * r)
        text(0.8, 0.8, Signif, cex = cex, col = 2)
    }
    f <- function(t) dnorm(t, mean = mean(x), sd = sd(x))
    # Useful function for histogram showing density overlay and rug
    hist.panel = function(x, ...) {
        par(new = TRUE)
        hist(x, col = "light gray", probability = TRUE, axes = FALSE, main = "", breaks = "FD")
        lines(density(x, na.rm = TRUE), col = "red", lwd = 1)
        rug(x)
    }
    if (histogram) pairs(x, gap = gap, lower.panel = panel.smooth, upper.panel = panel.cor, diag.panel = hist.panel, ...)
    else           pairs(x, gap = gap, lower.panel = panel.smooth, upper.panel = panel.cor, ...)
}
