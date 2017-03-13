#' Character strings from unquoted names
#' 
#' Makes a vector of character strings from a list of valid names. Taken from Hmisc.
#' 
#' @author Frank Harrell
#'
#' @param ... any number of names separated by commas
#'
#' @return character string vector
#'
#' @examples
#' Cs(a, cat, dog)
#' Cs(age, sex, race, bloodpressure, height)
#' Cs(you, must, quote, "things with spaces", 'or invalid ! @#$ characters')
#' 
#' @export
Cs <- function(...) {
    as.character(sys.call())[-1]
}