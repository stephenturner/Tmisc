#' Plot missing data
#' 
#' Plots missing data as holes on a black canvas.
#' 
#' @author Stephen Turner
#' @keywords NA
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

