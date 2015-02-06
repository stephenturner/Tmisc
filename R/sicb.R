#' Write sessionInfo to the clipboard
#' 
#' Writes output of \code{sessionInfo()} to the clipboard. Only works on Mac.
#' 
#' @author Stephen Turner
#' @keywords sessioninfo
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' # Write sessionInfo() to the clipboard on mac.
#' sicb()
#' }

sicb <- function() {
    # Check to make sure you're running on mac.
    if (Sys.info()[1]=="Darwin") {
        capture.output(sessionInfo(), file=pipe("pbcopy"))
    } else {
        message("This only works on mac.")
    }
}
