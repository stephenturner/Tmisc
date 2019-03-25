#' Keep rows from top groups of a column
#' 
#' Extract rows belonging to top n groups of a certain column 
#' 
#' @author Jeroen Janssens
#'
#' @param .data The data frame to operate on
#' @param col A formula indicating the column to group over
#' @param n The number of top groups to extract
#' 
#' @importFrom dplyr semi_join count_
#' 
#' @return A data frame conaining only rows belonging to the top n groups of the column
#'
#' @examples
#' \dontrun{
#' # All the cars
#' (nrow(mtcars))
#' # Only those in the top 2 groups based on the # carbs
#' nrow(keep_top_n(mtcars, ~carb, n=2))
#' }
#' @export
keep_top_n <- function(.data, col, n = 10) {
    .Deprecated("top_n()")
    semi_join(.data, head(count_(.data, col, sort = TRUE), n))
}
