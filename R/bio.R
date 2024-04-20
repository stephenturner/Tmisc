#' Reverse complement
#'
#' Reverse complements a sequence.
#'
#' @param x A sequence to reverse complement
#'
#' @return The sequence, reverse complemented
#' @export
#'
#' @examples
#' revcomp("GATTACA")
#' sapply(c("GATTACA", "CATATTAC"), revcomp)
#'
revcomp <- function(x) {
    if (!(is.character(x)&&length(x)==1L&&!is.na(x))) stop("x should be a single string.")
    spl <- strsplit(x, split="")[[1]]
    if (any(!spl %in% c("A", "C", "G", "T", "a", "c", "g", "t"))) stop("Found a non-ACGT nucleotide.")
    rc <-
        spl |>
        chartr("ACGTacgt", "TGCAtgca", x=_) |>
        rev() |>
        paste(collapse="")
    return(rc)
}
