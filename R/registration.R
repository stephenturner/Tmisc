#' Get registration link
#' 
#' Returns a link to register for a course. This is used to ensure that
#' participants were able to successfully download and install R and Rstudio
#' before coming to the course.
#' 
#' @param x Keep \code{NULL} for now.
#'   
#' @return A registration link
#'   
#' @keywords internal
#'   
#' @export
#' 
#' @examples
#' registration()

registration <- function(x=NULL) {
    message("To continue registration, please take the survey at:")
    message("http://bit.ly/intro-r-lifesci-survey")
}