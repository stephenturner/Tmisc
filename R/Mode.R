#' Mode.
#' 
#' Returns the mode of a vector. First in a tie wins (see examples).
#' 
#' @author Stephen Turner
#' @keywords keywords
#'   
#' @param x A vector.
#' @param na.rm Remove missing values before calculating the mode (FALSE by
#'   default). NAs are counted just like any other element. That is, an NA in
#'   the vector won't necessarily result in a return NA. See the first example.
#'   
#' @return A combined p-value.
#'   
#' @examples
#' Mode(c(1,2,2,3,3,3, NA))
#' Mode(c(1,2,2,3,3,3, NA), na.rm=TRUE)
#' Mode(c(1,2,2,3,3,3, NA, NA, NA, NA))
#' Mode(c(1,2,2,3,3,3, NA, NA, NA, NA), na.rm=TRUE)
#' Mode(c("A", "Z", "Z", "B", "B"))
#' 
#' @export
Mode <- function(x, na.rm=FALSE) {
    if (!is.vector(x)) stop("x is not a vector.")
    if(na.rm) x <- x[!is.na(x)]
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}
