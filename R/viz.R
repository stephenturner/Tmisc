#' Better scatterplot matrices.
#' 
#' A matrix of scatter plots with rugged histograms, correlations, and significance stars. Much of the functionality borrowed from \code{PerformanceAnalytics::chart.Correlation()}.
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


#' Plot missing data
#' 
#' Plots missing data as holes on a black canvas.
#' 
#' @param df A data.frame.
#' 
#' @examples
#' # What a mess. 
#' # Feature 10 is missing a lot. Observations 25 and 35 are completely missing.
#' # Most of features 40-45 are missing, except for the first few observations.
#' set.seed(2016-07-12)
#' x <- matrix(1, nrow=50, ncol=50)
#' x[sample(prod(dim(x)), 100)] <- NA
#' x <- data.frame(x)
#' x$X10[sample(length(x$X10), 25)] <- NA
#' x[c(25, 35), ] <- NA
#' x[1:40, 40:45] <- NA
#' gg_na(x)
#' 
#' @export
gg_na <- function(df) {
    stopifnot("data.frame" %in% class(df))
    if (!requireNamespace("ggplot2", quietly = TRUE) | !requireNamespace("reshape2", quietly = TRUE)) {
        stop("ggplot2 and reshape2 packages needed for this function to work. Please install them.", call. = FALSE)
    } else {
        df <- reshape2::melt(is.na(df))
        ggplot2::ggplot(df, ggplot2::aes_string(x = "Var2", y = "Var1")) +
            ggplot2::geom_raster(ggplot2::aes_string(fill = "value")) +
            ggplot2::scale_fill_grey(name = "", labels = c("Present","Missing")) +
            ggplot2::theme_classic() +
            ggplot2::theme(axis.text.x  = ggplot2::element_text(angle=90, hjust=1),
                           axis.ticks.x = ggplot2::element_blank()) +
            ggplot2::labs(x = "Columns / Variables", y = "Rows / observations")
    }
}


#' Emulate ggplot2 default hues
#' 
#' This will emulate ggplot2's hues, which are equally spaced hues around the color wheel, starting from 15.
#' 
#' @param n The Numeric; number of hues to generate.
#' @param start Numeric; the place on the color wheel to start. ggplot2 default is 15.
#'   
#' @return A vector of hues
#' 
#' @examples
#' n <- 10
#' gghues(3)
#' barplot(rep(1,n), col=gghues(n), names=gghues(n))
#' barplot(rep(1,n), col=gghues(n, start=15+180), names=gghues(n, start=15+180))
#' 
#' @export
gghues <- function(n, start=15) {
    hues = seq(start, 360+start, length = n + 1)
    grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
}



#' Histograms with overlays
#' 
#' Plot a histogram with either a normal distribution or density curve overlay.
#' 
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

