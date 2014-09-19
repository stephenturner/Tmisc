#' Open the current working directory on mac
#' 
#' Open the current working directory on mac
#' 
#' @author Stephen Turner
#' @keywords keywords
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' o()
#' }

o <- function() {
    if(Sys.info()[1]=="Darwin") {
        message(getwd())
        system("open .")
    } else {
        warning("This only works on Mac.")
    }
}