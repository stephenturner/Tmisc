#' Install packages
#' 
#' Shortcut to \code{\link{install.packages}}
#' 
#' @author Stephen Turner
#' @keywords keywords
#' 
#' @param ... Arguments to be passed to \code{install.packages}
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' lsp(Tmisc, pattern="un")
#' }

ipak <-function(...) install.packages(...)
