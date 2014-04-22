#' List objects and classes
#' 
#' List all the objects and their classes in the global environment.
#' 
#' @return A data frame with all objects and their class.
#' 
#' @keywords keywords
#' 
#' @export
#' 
#' @examples
#' # lsa()

lsa <- function() {
    obj_type <- function(x) class(get(x, envir = .GlobalEnv)) # define environment
    foo = data.frame(sapply(ls(envir = .GlobalEnv), obj_type))
    foo$object_name = rownames(foo)
    names(foo)[1] = "class"
    names(foo)[2] = "object"
    return(unrowname(foo))
}
