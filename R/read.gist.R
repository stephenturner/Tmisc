#' Read data from a github gist
#' 
#' Read data from a github gist. Modified from Hadley Wickham's
#' \code{source_gist()} function in devtools. Call the function with the gist
#' ID. E.g., if url is \url{https://gist.github.com/user/abc123}, the ID is
#' abc123.
#' 
#' @param gistid A GitHub gist ID.
#' @param ... Other arguments passed to \code{read.table()}
#'   
#' @return A data frame with the data you read in from the github gist.
#'   
#' @keywords keywords
#'   
#' @export
#' 
#' @examples
#' # read.gist("6fa823bb2de7e541ad54", header=TRUE)

read.gist <- function(gistid, ...) {
    
    require(httr)
    stopifnot(length(id)==1, is.character(id))
    
    # Use the github api to retrieve the raw URL
    ## Get the API url
    url <- sprintf("https://api.github.com/gists/%s", id)
    req <- GET(url)
    stop_for_status(req)
    text <- content(req, "text")
    ## Yuck, parse the JSON with a regex.
    url_pos <- regexec('"raw_url": ?"(.*?)"', text)
    matches <- regmatches(text, url_pos)[[1]]
    stopifnot(length(matches)==2)
    ## This is the raw github url.
    rawurl <- matches[2]
    
    # read data from the raw url
    req <- GET(rawurl)
    stop_for_status(req)
    handle <- textConnection(content(req, as = 'text'))
    on.exit(close(handle))
    read.table(handle, ...)
}