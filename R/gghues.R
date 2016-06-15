#' Emulate ggplot2 default hues
#' 
#' This will emulate ggplot2's hues, which are equally spaced hues around the color wheel, starting from 15.
#' 
#' @author Stephen Turner
#' @keywords keywords
#'   
#' @param n The Numeric; number of hues to generate.
#' @param start Numeric; the place on the color wheel to start. ggplot2 default is 15.
#'   
#' @return A vector of hues
#' 
#' @importFrom grDevices hcl
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
    hcl(h = hues, l = 65, c = 100)[1:n]
}
