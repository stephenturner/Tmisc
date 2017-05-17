#' @title x not in y
#' @description Returns a logical vector of elements of x that are not in y.
#' @param x a vector (numeric, character, factor)
#' @param table a vector (numeric, character, factor), matching the mode of x
#' @return A logical vector with length equal to \code{x} of things in \code{x} that aren't in \code{y}.
#' @seealso \code{\link{\%like\%}}, \code{\link{\%nlike\%}}, \code{\link{\%nin\%}}, 
#' @examples
#' 1:10 %nin% seq(from=2, to=10, by=2)
#' c("a", "b", "c") %nin% c("a", "b")
#' letters[letters %nin% unlist(strsplit("pack my box with five dozen liquor jugs", ""))]
#' @export
`%nin%` <- function (x, table) match(x, table, nomatch = 0) == 0


#' @title  x like y
#' @description Returns a logical vector of elements of x matching the regex y.
#' @param x a vector (numeric, character, factor)
#' @param pattern a vector (numeric, character, factor), matching the mode of x
#' @return A logical vector with length equal to \code{x} of things in \code{x} that are like \code{y}.
#' @seealso \code{\link{\%like\%}}, \code{\link{\%nlike\%}}, \code{\link{\%nin\%}}, 
#' @examples
#' (Name <- c("Mary","George","Martha"))
#' Name %in% c("Mary")
#' Name %like% "^Mar"
#' Name %nin% c("George")
#' Name %nlike% "^Mar"
#' @export
`%like%` <- function(x, pattern)
{
    if (is.factor(x)) {
        as.integer(x) %in% grep(pattern,levels(x))
    } else {
        grepl(pattern,x)
    }
}


#' @title  x not like y
#' @description Returns a logical vector of elements of x not matching the regex y.
#' @param x a vector (numeric, character, factor)
#' @param pattern a vector (numeric, character, factor), matching the mode of x
#' @return A logical vector with length equal to \code{x} of things in \code{x} that aren't like \code{y}.
#' @seealso \code{\link{\%like\%}}, \code{\link{\%nlike\%}}, \code{\link{\%nin\%}}, 
#' @examples 
#' (Name <- c("Mary","George","Martha"))
#' Name %in% c("Mary")
#' Name %like% "^Mar"
#' Name %nin% c("George")
#' Name %nlike% "^Mar"
#' @export
`%nlike%` <- function(x, pattern) {
    !(x %like% pattern)
}
