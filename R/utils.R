#' Are all equal? 
#' 
#' Are all the elements of a numeric vector (approximately) equal?
#' 
#' @param x A numeric vector.
#' @param na.rm Remove missing values (FALSE by default; NAs in x will return NA).
#' 
#'   
#' @return Logical, whether all elements of a numeric vector are equal.
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
#' @export
are_all_equal <- function(x, na.rm=FALSE) {
    stopifnot(is.numeric(x))
    if(na.rm) x <- x[!is.na(x)]
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
#' 
#' @importFrom stats na.omit
#'   
#' @examples
#' lowestnonzero(c(.042, .02, 0, .001, 0, .89))
#' 
#' @export
lowestnonzero <- function(x) {
    if(any(na.omit(x)<0) | any(na.omit(x)>1)) stop("P-values should be between 0 and 1.")
    xo <- x[order(x)]
    lnz <- xo[xo>0][1]
    x[x==0] <- lnz
    x
}



#' Improved list of objects
#' 
#' Improved list of objects.  Sorts by size by default. Adapted from \url{https://stackoverflow.com/q/1358003/654296}.
#' 
#' @author Dirk Eddelbuettel, Tony Breyal
#' @keywords NA
#'   
#' @param pos numeric. Position in the stack.
#' @param pattern Regex to filter the objects by.
#' @param order.by character. Either 'Type', 'Size', 'PrettySize', 'Rows', or 
#'   'Columns'. This will dictate how the output is ordered.
#' @param decreasing logical. Should the output be displayed in decreasing order?
#' @param head logical. Use head on the output?
#' @param n numeric. Number of objects to display is head is TRUE.
#'   
#' @return A data.frame with type, size in bytes, human-readable size, rows, and
#'   columns of every object in the environment.
#' @import utils
#' 
#' @examples
#' \dontrun{
#' a <- rnorm(100000)
#' b <- matrix(1, 1000, 100)
#' lsa()
#' }
#'   
#' @export
lsa <- function (pos = 1, pattern, order.by = "Size",
                 decreasing=TRUE, head=TRUE, n=10) {
    napply <- function(names, fn) sapply(names, function(x)
        fn(get(x, pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.prettysize <- napply(names, function(x) {
        capture.output(print(object.size(x), units = "auto")) })
    obj.size <- napply(names, object.size)
    obj.dim <- t(napply(names, function(x)
        as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
    names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
    if (!missing(order.by))
        out <- out[order(out[[order.by]], decreasing=decreasing), ]
    if (head)
        out <- head(out, n)
    out
}



#' Open the current working directory on mac
#' 
#' Opens the current working directory on mac.
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
    } else {
        warning("This only works on Mac.\nKnow how to use Windows? Submit a PR at:\nhttps://github.com/stephenturner/Tmisc/.")
    }
}



#' Peek at the top of a text file
#' 
#' This returns a character vector which shows the top n lines of a file.
#' 
#' @param x a filename
#' @param n the number of lines to return
#'   
#' @examples
#' \dontrun{
#' filename <- tempfile()
#' x<-matrix(round(rnorm(10^4),2),1000,10)
#' colnames(x)=letters[1:10]
#' write.csv(x,file=filename,row.names=FALSE)
#' peek(filename)
#' }
#' @export
peek <- function(x,n=5) scan(x,what="char",n=n,sep="\n")



#' Write sessionInfo to the clipboard
#' 
#' Writes output of \code{sessionInfo()} to the clipboard. Only works on Mac.
#' 
#' @import utils
#' 
#' @examples
#' \dontrun{
#' # Write sessionInfo() to the clipboard on mac.
#' sicb()
#' }
#' 
#' @export
sicb <- function() {
    # Check to make sure you're running on mac.
    if (Sys.info()[1]=="Darwin") {
        capture.output(sessionInfo(), file=pipe("pbcopy"))
    } else {
        warning("This only works on Mac.\nKnow how to use Windows? Submit a PR at:\nhttps://github.com/stephenturner/Tmisc")
    }
}



#' Sort characters in a string
#' 
#' Alphabetically sorts characters in a string. Vectorized over x.
#' 
#' @param x A string to sort.
#'   
#' @return A sorted string.
#'   
#' @examples
#' strSort("cba")
#' strSort("zyxcCbB105.a")
#' strSort(c("cba", "zyx"))
#' strSort(c("cba", NA))
#' 
#' @export
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
#' @examples
#' \dontrun{
#' foo <- 1
#' saveit(bar=foo, file="foobar.Rdata")
#' }
#'   
#' @export
saveit <- function(..., file=stop("'file' must be specified")) {
    x <- list(...)
    save(list=names(x), file=file, envir=list2env(x))
}



#' @title  Two-letter genotype from VCF GT
#' @description Get a two-letter genotype from a VCF GT field. Current
#'   implementation is quick and dirty, and only accepts 0/0, 0/1, or 1/1. Any
#'   other input to gt will return a missing value.
#' @param gt The genotype field (must be 0/0, 0/1, or 1/1).
#' @param ref The reference allele.
#' @param alt The alternate allele.
#' @return Returnvalue
#' @importFrom dplyr case_when
#' @examples
#' gt2refalt(gt="0/0", ref="R", alt="A")
#' gt2refalt(gt="0/1", ref="R", alt="A")
#' gt2refalt(gt="1/1", ref="R", alt="A")
#' gt2refalt(gt="0/2", ref="R", alt="A")
#' gt2refalt(gt="./.", ref="R", alt="A")
#' @export
gt2refalt <- function(gt, ref, alt) {
    dplyr::case_when(gt=="0/0" ~ paste0(ref,ref), 
                     gt=="0/1" ~ paste0(ref,alt), 
                     gt=="1/0" ~ paste0(ref,alt), 
                     gt=="1/1" ~ paste0(alt,alt))
}
