#' Create tables in dokuwiki format
#' 
#' Prints the supplied data frame or matrix using Dokuwiki's table syntax, optionally copying the data to the clipboard (Mac OS X only).
#' 
#' @param x A data.frame.
#' @param headersep The separator used between entries in the header row.
#' @param sep The separator used between entries in all other rows.
#' @param clip Whether or not to write the returned table to the clipboard (currently only supported on Mac OS X). 
#' @param ... Further arguments passed to \code{write.table}.
#' 
#' @examples
#' dokuwiki(head(iris), clip=FALSE)
#' dokuwiki(head(mtcars), clip=FALSE, row.names=TRUE)
#' 
#' @export
dokuwiki <- function(x, headersep="^", sep="|", clip=TRUE, ...) {
    .dots <- list(...)
    .dots$x <- x
    .dots$sep <- sep
    .dots$col.names <- FALSE
    .dots$row.names <- ifelse(is.null(.dots$row.names), FALSE, .dots$row.names)
    .dots$quote <- ifelse(is.null(.dots$quote), FALSE, .dots$quote)
    .dots$na <- ifelse(is.null(.dots$na), "", .dots$na)
    # Header row. If printing row.names, add an extra header separator column
    if (.dots$row.names) {
        row1 <- paste0(headersep, " ", headersep, paste(colnames(x), collapse=headersep), headersep, "\n")
    } else {
        row1 <- paste0(headersep, paste(colnames(x), collapse=headersep), headersep, "\n")
    }
    # All other rows
    otherrows <- paste0(sep, utils::capture.output(do.call(utils::write.table, .dots)), sep, collapse = "\n")
    allrows <- paste0(row1, otherrows, collapse="\n")
    if (clip) {
        if (Sys.info()["sysname"]=="Darwin") {
            con <- pipe("pbcopy")
            writeChar(allrows, con=con, eos=NULL)
            close(con)
            message("Copied to clipboard:\n")
        } else {
            warning("Writing to clipboard is supported on Mac OS X only.")
        }
    }
    cat(allrows)
}


#' Print the top left corner of a data frame
#' 
#' Prints the first n rows and columns of a data frame or matrix.
#' 
#' @param x A data.frame.
#' @param n The number of rows/columns to print.
#' @return The corner of the data frame
#' 
#' @examples
#' corner(mtcars)
#' corner(iris, n=4)
#' 
#' @export
corner <- function(x, n=5) {
    if(is.data.frame(x)|is.matrix(x)) {
        if (n>nrow(x)) warning("Specified 'n' is greater than the number of rows.")
        if (n>ncol(x)) warning("Specified 'n' is greater than the number of columns.")
        n <- min(n, nrow(x), ncol(x))
        x[1:n,1:n]
    } else {
        stop(paste(deparse(substitute(x)), "is not a matrix or data.frame."))
    }
}


#' Truncate a data frame with ellipses.
#' 
#' Prints the specified number of rows of a data frame, followed by a row of ellipses. Useful for piping to \code{knitr::kable()} for printing a truncated table in a markdown document.
#' 
#' @param df A data.frame.
#' @param n The number of rows to show before an ellipses row.
#' @return A data frame truncated by a row of ellipses.
#' 
#' @examples
#' \dontrun{
#' ellipses(mtcars, 5)
#' }
#' 
#' @export
ellipses <- function(df, n=5L) {
    stopifnot("data.frame" %in% class(df))
    els <- rep("...", ncol(df)) %>% 
        matrix(nrow=1, dimnames=list(NULL, names(df))) %>% 
        data.frame(stringsAsFactors=FALSE) %>% 
        tibble::as_tibble()
    out <- df %>% 
        head(n) %>% 
        lapply(as.character) %>% 
        data.frame(stringsAsFactors=FALSE) %>% 
        tibble::as_tibble() %>% 
        dplyr::bind_rows(els)
    return(out)
}

#' Get names and class of all columns in a data frame
#' 
#' Get names and class of all columns in a data frame in a friendly format.
#' 
#' @author Stephen Turner
#' @keywords NA
#' 
#' @param df A data.frame.
#' 
#' @return A data.frame with index and class.
#' 
#' @examples
#' nn(iris)
#' 
#' @export
nn <- function(df) data.frame(var=names(df), 
                              index=1:ncol(df), 
                              class=sapply(df, class), 
                              row.names=NULL)

#' Missing stats
#' 
#' Returns the number of missing values, total length, and proportion
#' missing values for each variable in a data.frame
#' 
#' @param df A data.frame.
#' @return A data.frame with missingness stats.
#' 
#' @examples
#' \dontrun{
#' propmiss(data.frame(a=1:5, b=c(6,NA,NA,9,10)))
#' }
#' 
#' @export
propmiss <- function(df) {
    .Deprecated("Use summarize(across(everything(), ~sum(is.na(.))/n())")
    m <- sapply(df, function(x) {
        data.frame(
            nmiss=sum(is.na(x)), 
            n=length(x), 
            propmiss=sum(is.na(x))/length(x)
        )
    })
    d <- data.frame(t(m))
    d <- sapply(d, unlist)
    d <- as.data.frame(d)
    d$var <- row.names(d)
    row.names(d) <- NULL
    d <- cbind(d[ncol(d)],d[-ncol(d)])
    d <- tibble::as_tibble(d)
    return(d)
}



#' Matrix to pairwise data frame
#' 
#' Turns a distance matrix into a data frame of pairwise distances.
#' 
#' @param M a square pairwise matrix (e.g., of distances).
#' @return Data frame with pairwise distances.
#' @examples
#' set.seed(42)
#' M <- matrix(rnorm(25), nrow=5)
#' M
#' mat2df(M)
#' M <- matrix(rnorm(25), nrow=5, dimnames=list(letters[1:5], letters[1:5]))
#' M
#' mat2df(M)
#'   
#' @export
mat2df <- function(M) {
    if (!methods::is(M, "matrix")) stop("M must be a square matrix. (M is not a matrix).")
    if (nrow(M)!=ncol(M))   stop("M must be a square matrix. (M is not square).")
    if (is.null(colnames(M))) colnames(M) <- 1:ncol(M)
    if (is.null(rownames(M))) rownames(M) <- 1:ncol(M)
    if (!identical(rownames(M), colnames(M))) stop("rownames(M) != colnames(M)")
    xy <- t(combn(colnames(M), 2))
    data.frame(id1=xy[,1], id2=xy[,2], value=M[xy], stringsAsFactors = FALSE)
}

