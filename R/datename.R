#' Filename with date
#' 
#' Returns string with today's date in YYYY-MM-DD- format concatenated to filename.
#' 
#' @param filename A filename string
#' 
#' @return String with today's date in YYYY-MM-DD- format concatenated to filename.
#' 
#' @keywords keywords
#' 
#' @export

datename <- function(filename="filename") paste0(Sys.Date(), "-", filename)