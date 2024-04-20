#' Better scatterplot matrices
#' 
#' A matrix of scatter plots with rugged histograms, correlations, and significance stars. Much of the functionality borrowed from `PerformanceAnalytics::chart.Correlation()`.
#' 
#' @param x A numeric matrix or data.frame.
#' @param histogram Overlay a histogram on the diagonals?
#' @param gap distance between subplots, in margin lines.
#' @param ... arguments to be passed to or from other methods.
#' 
#' @importFrom stats cor cor.test density dnorm sd symnum
#' @importFrom graphics hist lines pairs panel.smooth par rug strwidth text
#' 
#' @export
#' 
#' @examples
#' Tpairs(iris[-5])
#' Tpairs(iris[-5], pch=21, bg=gghues(3)[factor(iris$Species)], gap=1)
#' 
Tpairs <- function (x, histogram = TRUE, gap=0, ...) 
{
    x <- as.matrix(x)
    if (mode(x)!="numeric") stop("Must pass in only numeric values")
    panel.cor <- function(x, y, digits = 2, prefix = "", use = "pairwise.complete.obs", cex.cor, ...) {
        usr <- par("usr")
        on.exit(par(usr=usr))
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


#' Plot missing data
#' 
#' Plots missing data as holes on a black canvas.
#' 
#' @param df A data frame
#' 
gg_na <- function(df) {
    .Deprecated("visdat::vis_dat") 
}


#' Emulate ggplot2 default hues
#' 
#' This will emulate ggplot2's hues, which are equally spaced hues around the color wheel, starting from 15.
#' 
#' @param n The Numeric; number of hues to generate.
#' @param start Numeric; the place on the color wheel to start. ggplot2 default is 15.
#'   
#' @return A vector of hues
#' @export
#' 
#' @examples
#' n <- 10
#' gghues(3)
#' barplot(rep(1,n), col=gghues(n), names=gghues(n))
#' barplot(rep(1,n), col=gghues(n, start=15+180), names=gghues(n, start=15+180))
#' 
gghues <- function(n, start=15) {
    hues = seq(start, 360+start, length = n + 1)
    grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
}


#' Histograms with overlays
#' 
#' Plot a histogram with either a normal distribution or density curve overlay.
#' 
#' @param x A numeric vector.
#' @param overlay Either "normal" (default) or "density" indicating whether a normal distribution or density curve should be plotted on top of the histogram.
#' @param col Color of the histogram bars.
#' @param ... Other arguments to be passed to [hist()].
#' 
#' @importFrom stats density dnorm na.omit
#' @importFrom graphics hist rug lines
#' 
#' @export
#' 
#' @examples
#' set.seed(42)
#' x <- rnorm(1000, mean=5, sd=2)
#' Thist(x)
#' Thist(x, overlay="density")
#' Thist(x^2)
#' Thist(x^2, overlay="density", breaks=50, col="lightblue2")
#' 
Thist <- function(x, overlay="normal", col="gray80", ...) {
    if (!is.numeric(x) | !is.vector(x)) stop("x must be a numeric vector")
    if (!overlay %in% c("normal", "density")) stop("overlay must be 'normal' or 'density'")
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

