#' Rename objects while saving.
#' 
#' Allows you to rename objects as you save them. See
#' \url{http://stackoverflow.com/a/21248218/654296}.
#' 
#' @author Stephen Turner
#' @keywords NA
#'   
#' @param ... Objects to save.
#' @param file Filename/path where data will be saved.
#'   
#' @export
#' 
#' @examples
#' \dontrun{
#' foo <- 1
#' saveit(bar=foo, file="foobar.Rdata")
#' }
 
saveit <- function(..., file=stop("'file' must be specified")) {
    x <- list(...)
    save(list=names(x), file=file, envir=list2env(x))
}
