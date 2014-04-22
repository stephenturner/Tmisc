#' List objects in package
#' 
#' List all the objects in a package. Orig Karthik Ram.
#' 
#' @param packagename The name of the package you're examining.
#' @param all.names a logical value. If TRUE, all object names are returned. If FALSE, names which begin with a . are omitted.
#' @param pattern an optional regular expression. Only names matching pattern are returned. glob2rx can be used to convert wildcard patterns to regular expressions.
#' 
#' @return A character vector of all the objects in the specified package.
#' 
#' @keywords keywords
#' 
#' @export

lsp <-function(packagename, all.names = FALSE, pattern) {
    package <- deparse(substitute(package))
    ls(
        pos = paste("package", package, sep = ":"),
        all.names = all.names,
        pattern = pattern
    )
}
