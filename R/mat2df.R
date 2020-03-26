#' Matrix to pairwise data frame
#' 
#' Turns a distance matrix into a data frame of pairwise distances.
#' 
#' @author Stephen Turner
#'   
#' @param M a square pairwise matrix (e.g., of distances).
#'   
#' @return Data frame with pairwise distances.
#' 
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
    if (!is(M, "matrix")) stop("M must be a square matrix. (M is not a matrix).")
    if (nrow(M)!=ncol(M))   stop("M must be a square matrix. (M is not square).")
    if (is.null(colnames(M))) colnames(M) <- 1:ncol(M)
    if (is.null(rownames(M))) rownames(M) <- 1:ncol(M)
    if (!identical(rownames(M), colnames(M))) stop("rownames(M) != colnames(M)")
    xy <- t(combn(colnames(M), 2))
    data.frame(id1=xy[,1], id2=xy[,2], value=M[xy], stringsAsFactors = FALSE)
}
