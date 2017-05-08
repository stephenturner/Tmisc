#' Datasaurus Dozen
#' 
#' The Datasaurus Dozen dataset
#' 
#' @source Justin Matejka, George Fitzmaurice. "Same Stats, Different Graphs:
#'   Generating Datasets with Varied Appearance and Identical Statistics through
#'   Simulated Annealing." 2017 ACM SIGCHI Conference on Human Factors in
#'   Computing Systems. \url{https://www.autodeskresearch.com/publications/samestats}.
#' @format Data frame with columns.
#' @examples 
#' \dontrun{
#' library(dplyr)
#' datasaurus %>% 
#'   group_by(set) %>% 
#'   summarize(mean(x), mean(y), sd(x), sd(y), cor(x, y))
#' library(ggplot2)
#' ggplot(datasaurus, aes(x,y)) + 
#'   geom_point() + 
#'   geom_smooth(method="lm") + 
#'   facet_wrap(~set)
#' }

"datasaurus"