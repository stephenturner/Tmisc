#' Mode.
#' 
#' Returns the mode of a vector. First in a tie wins (see examples).
#' 
#' @author Stephen Turner
#' @keywords keywords
#'   
#' @param x A vector.
#' @param na.rm Remove missing values before calculating the mode (TRUE by default).
#'   
#' @return A combined p-value.
#' 
#' @importFrom stats na.omit pchisq
#'   
#' @examples
#' Mode(c(1,2,2,3,3,3,10))
#' Mode(c("A", "C", "C", "B", "B"))
#' Mode(c(10,20,20,30,30,30,NA, NA, NA, NA, NA, 100))
#' Mode(c(1,2,2,3,3,3,NA, NA, NA, NA, NA, 10), na.rm=FALSE)
#' 
#' @export
Mode <- function(x, na.rm=TRUE) {
    if (!is.vector(x)) stop("x is not a vector.")
    if(na.rm) x <- x[!is.na(x)]
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}
