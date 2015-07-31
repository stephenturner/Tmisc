#' Read from the clipboard
#' 
#' Read tabular data from the clipboard.
#' 
#' @author Stephen Turner
#' @keywords keywords
#' 
#' @param ... Arguments to be passed to \code{read.table}
#' 
#' @return A data.frame
#' 
#' @export
#' @import utils
#' 
#' @examples
#' \dontrun{
#' # To read CSV data with a header from the clipboard:
#' read.cb(header=TRUE, sep=',')
#' }

read.cb <- function(...) {
    ismac <- Sys.info()[1]=="Darwin"
    if (!ismac) read.table(file="clipboard", ...)
    else read.table(pipe("pbpaste"), ...)
}