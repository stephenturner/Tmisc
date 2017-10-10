#' @title Insert text at current position.
#' @name addins
#' @description Call these function as an addin to insert desired text at the
#'   cursor position. After installing Tmisc, hit the Addins menu, and
#'   optionally add a keyboard shortcut, e.g., Command+Shift+I, Alt+-, etc.
#'   
#' @rdname addins
#' @usage NULL
#' @export
insertInAddin <- function() {
    rstudioapi::insertText(" %in% ")
}
#' @rdname addins
#' @usage NULL
#' @export
insertEqual <- function() {
    rstudioapi::insertText(" = ")
}
