#' Get registration link
#' 
#' Returns a link to register for a course. This is used to ensure that
#' participants were able to successfully download and install R and Rstudio
#' before coming to the course.
#' 
#' @author Stephen Turner
#' @keywords keywords
#' 
#' @param x Keep \code{NULL} for now.
#'   
#' @return A registration link
#'   
#' @export
#' 
#' @examples
#' registration()

registration <- function(x=NULL) {
    message("Please visit the following link to register:")
    message("http://stephenturner.us")
}