#' Insert \%in\%.
#' 
#' Call this function as an addin to insert \code{ \%in\% } at the cursor
#' position. After installing Tmisc, hit the Addins menu, and optionally add a
#' keyboard shortcut, e.g., Command-Shift-I.
#' 
#' @export
insertInAddin <- function() {
    rstudioapi::insertText(" %in% ")
}
