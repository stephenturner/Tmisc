#' Print the top left corner of a data frame
#' 
#' Prints the first n rows and columns of a data frame or matrix.
#' 
#' @author Stephen Turner
#' 
#' @param x A data.frame.
#' @param n The number of rows/columns to print.
#' 
#' @return The corner of the data frame
#' 
#' @examples
#' corner(mtcars)
#' corner(iris, n=4)
#' 
#' @export
corner <- function(x, n=5) {
    if(is.data.frame(x)|is.matrix(x)) {
        if (n>nrow(x)) warning("Specified 'n' is greater than the number of rows.")
        if (n>ncol(x)) warning("Specified 'n' is greater than the number of columns.")
        n <- min(n, nrow(x), ncol(x))
        x[1:n,1:n]
    } else {
        stop(paste(deparse(substitute(x)), "is not a matrix or data.frame."))
    }
}
