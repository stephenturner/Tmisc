#' Print the top left corner of a data frame
#' 
#' Prints the first n rows and columns of a data frame or matrix.
#' 
#' @param x A data.frame.
#' @param n The number of rows/columns to print.
#' 
#' @return The corner of the data frame
#' @export
#' 
#' @examples
#' corner(mtcars)
#' corner(iris, n=4)
#' 
corner <- function(x, n=5) {
    if (!is.data.frame(x) && !is.matrix(x)) stop("Input must be a matrix or data frame")
    if (n>nrow(x)) warning("Specified 'n' is greater than the number of rows.")
    if (n>ncol(x)) warning("Specified 'n' is greater than the number of columns.")
    n <- min(n, nrow(x), ncol(x))
    x[1:n,1:n]
}


#' Truncate a data frame with ellipses.
#' 
#' Prints the specified number of rows of a data frame, followed by a row of
#' ellipses. Useful for piping to `knitr::kable()` for printing a truncated
#' table in a markdown document.
#' 
#' @param df A data frame.
#' @param n The number of rows to show before an ellipses row.
#' 
#' @return A data frame truncated by a row of ellipses.
#' @export
#' 
#' @examples
#' ellipses(mtcars, 5)
#' 
ellipses <- function(df, n=5L) {
    if (!inherits(df, "data.frame")) stop("df must be a data frame.")
    els <- rep("...", ncol(df)) |> 
        matrix(nrow=1, dimnames=list(NULL, names(df))) |> 
        data.frame(stringsAsFactors=FALSE) |> 
        tibble::as_tibble()
    out <- df |> 
        utils::head(n) |> 
        lapply(as.character) |> 
        data.frame(stringsAsFactors=FALSE) |> 
        tibble::as_tibble() |> 
        dplyr::bind_rows(els)
    return(out)
}

#' Get names and class of all columns in a data frame
#' 
#' Get names and class of all columns in a data frame in a friendly format.
#' 
#' @param df A data frame.
#' 
#' @return A data frame with index and class.
#' @export
#' 
#' @examples
#' nn(iris)
#' 
nn <- function(df) {
    if (!inherits(df, "data.frame")) stop("df must be a data frame.")
    tibble::tibble(var=names(df), 
                   index=1:ncol(df), 
                   class=sapply(df, class), 
                   row.names=NULL)
}

#' Matrix to pairwise data frame
#' 
#' Turns a distance matrix into a data frame of pairwise distances.
#' 
#' @param M a square pairwise matrix (e.g., of distances).
#' 
#' @return Data frame with pairwise distances.
#' @export
#' 
#' @examples
#' M <- matrix(1:25, nrow=5, dimnames=list(letters[1:5], letters[1:5]))
#' M
#'   
mat2df <- function(M) {
    if (!inherits(M, "matrix")) stop("M must be a square matrix. (M is not a matrix).")
    if (nrow(M)!=ncol(M)) stop("M must be a square matrix. (M is not square).")
    if (is.null(colnames(M))) colnames(M) <- 1:ncol(M)
    if (is.null(rownames(M))) rownames(M) <- 1:ncol(M)
    if (!identical(rownames(M), colnames(M))) stop("rownames(M) != colnames(M)")
    xy <- t(utils::combn(colnames(M), 2))
    tibble::tibble(id1=xy[,1], id2=xy[,2], value=M[xy])
}


#' Are all equal? 
#' 
#' Are all the elements of a numeric vector (approximately) equal?
#' 
#' @param x A numeric vector.
#' @param na.rm Remove missing values (FALSE by default; NAs in x will return NA).
#' 
#' @return Logical, whether all elements of a numeric vector are equal.
#' @export
#' 
#' @examples
#' are_all_equal(c(5,5,5))
#' are_all_equal(c(5,5,5,6))
#' are_all_equal(c(5,5,5,NA,6))
#' are_all_equal(c(5,5,5,NA,6), na.rm=TRUE)
#' 5==5.000000001
#' identical(5, 5.000000001)
#' are_all_equal(c(5L, 5, 5.000000001))
#' 
are_all_equal <- function(x, na.rm=FALSE) {
    if (!is.numeric(x)) stop("x must be numeric")
    if (na.rm) x <- x[!is.na(x)]
    abs(max(x)-min(x)) < .Machine$double.eps^.5
}



#' Lowest nonzero values
#' 
#' Sometimes want to plot p-values (e.g., volcano plot or MA-plot), but if a
#' statistical test returns a zero p-value, this causes problems with
#' visualization on the log scale. This function returns a vector where  the
#' zero values are equal to the smallest nonzero value in the vector.
#' 
#' @param x A vector of p-values between 0 and 1.
#'   
#' @return A vector of p-values where zero values are exchanged for the lowest non-zero p-value in the original vector.
#' @export
#' 
#' @examples
#' lowestnonzero(c(.042, .02, 0, .001, 0, .89))
#' 
lowestnonzero <- function(x) {
    if(any(stats::na.omit(x)<0) | any(stats::na.omit(x)>1)) stop("P-values should be between 0 and 1.")
    xo <- x[order(x)]
    lnz <- xo[xo>0][1]
    x[x==0] <- lnz
    x
}


#' Improved list of objects
#' 
#' Improved list of objects.  Sorts by size by default. Adapted from <https://stackoverflow.com/q/1358003/654296>.
#' 
#' @param pos numeric. Position in the stack.
#' @param pattern Regex to filter the objects by.
#' @param order.by character. Either 'Type', 'Size', 'PrettySize', 'Rows', or 'Columns'. This will dictate how the output is ordered.
#' @param decreasing logical. Should the output be displayed in decreasing order?
#' @param head logical. Use head on the output?
#' @param n numeric. Number of objects to display is head is TRUE.
#'   
#' @return A data.frame with type, size in bytes, human-readable size, rows, and
#'   columns of every object in the environment.
#' @export
#' 
#' @examples
#' a <- rnorm(100000)
#' b <- matrix(1, 1000, 100)
#' lsa()
#'   
lsa <- function (pos = 1, pattern, order.by = "Size", decreasing=TRUE, head=FALSE, n=10) {
    napply <- function(names, fn) sapply(names, function(x) fn(get(x, pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.prettysize <- napply(names, function(x) utils::capture.output(print(utils::object.size(x), units = "auto")))
    obj.size <- napply(names, utils::object.size)
    obj.dim <- t(napply(names, function(x) as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- 
        data.frame(obj.type, obj.size, obj.prettysize, obj.dim) |> 
        tibble::rownames_to_column("variable") |> 
        tibble::as_tibble()
    names(out) <- c("Variable", "Type", "Size", "PrettySize", "Rows", "Columns")
    if (!missing(order.by))
        out <- out[order(out[[order.by]], decreasing=decreasing), ]
    if (head)
        out <- utils::head(out, n)
    out
}



#' Open the current working directory on mac
#' 
#' @examples
#' \dontrun{
#' o()
#' }
#' 
#' @export
o <- function() {
    if(Sys.info()[1]=="Darwin") {
        message(getwd())
        system("open .")
    } 
}



#' Peek at the top of a text file
#' 
#' This returns a character vector which shows the top n lines of a file.
#' 
#' @param x a filename
#' @param n the number of lines to return
#' 
#' @return A character vector of the first n lines of the file.
#' @export
#' 
#' @examples
#' \dontrun{
#' filename <- tempfile()
#' x <- matrix(round(rnorm(10^4), 2), 1000, 10)
#' colnames(x) <- letters[1:10]
#' write.table(x, file = filename, row.names = FALSE, quote = FALSE)
#' peek(filename)
#' }
peek <- function(x,n=5) scan(x, what="char", n=n, sep="\n")


#' Write sessionInfo to the clipboard
#' 
#' Writes output of \code{sessionInfo()} to the clipboard. Only works on Mac.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' sicb()
#' }
#' 
sicb <- function() {
    if (Sys.info()[1]=="Darwin") {
        utils::capture.output(utils::sessionInfo(), file=pipe("pbcopy"))
    } 
}


#' Sort characters in a string
#' 
#' Alphabetically sorts characters in a string. Vectorized over x.
#' 
#' @param x A string to sort.
#'   
#' @return A sorted string.
#' @export
#'   
#' @examples
#' strSort("cba")
#' strSort("zyxcCbB105.a")
#' strSort(c("cba", "zyx"))
#' strSort(c("cba", NA))
#' 
strSort <- function(x) {
    ifelse(is.na(x), NA, sapply(lapply(strsplit(x, NULL), sort), paste, collapse=""))
}



#' Rename objects while saving.
#' 
#' Allows you to rename objects as you save them. See \url{https://stackoverflow.com/a/21248218/654296}.
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
#'   
saveit <- function(..., file=stop("'file' must be specified")) {
    x <- list(...)
    save(list=names(x), file=file, envir=list2env(x))
}



#' Two-letter genotype from VCF GT
#' 
#' Get a two-letter genotype from a VCF GT field. Current implementation is
#' quick and dirty, and only accepts 0/0, 0/1, or 1/1. Any other input to gt
#' will return a missing value.
#' 
#' @param gt The genotype field (must be 0/0, 0/1, or 1/1).
#' @param ref The reference allele.
#' @param alt The alternate allele.
#' 
#' @return Returnvalue
#' @export
#' 
#' @examples
#' gt2refalt(gt="0/0", ref="R", alt="A")
#' gt2refalt(gt="0/1", ref="R", alt="A")
#' gt2refalt(gt="1/1", ref="R", alt="A")
#' gt2refalt(gt="0/2", ref="R", alt="A")
#' gt2refalt(gt="./.", ref="R", alt="A")
#' 
#' @export
gt2refalt <- function(gt, ref, alt) {
    dplyr::case_when(gt=="0/0" ~ paste0(ref,ref), 
                     gt=="0/1" ~ paste0(ref,alt), 
                     gt=="1/0" ~ paste0(ref,alt), 
                     gt=="1/1" ~ paste0(alt,alt))
}
