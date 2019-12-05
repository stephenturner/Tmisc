#' Jensen-Shannon divergence
#' 
#' Calculates a distance matrix from a matrix of probability distributions using
#' Jensen-Shannon divergence. Adapted from \url{http://enterotype.embl.de/enterotypes.html#dm}.
#' 
#' @author Stephen Turner
#'   
#' @param M a probability distribution matrix, e.g., normalized transcript compatibility counts.
#' @param pseudocount a small number to avoid division by zero errors.
#' @param normalizeCounts logical, whether to attempt to normalize by dividing by the column sums. Set to \code{TRUE} if this is, e.g., a count matrix.
#'   
#' @return A Jensen-Shannon divergence-based distance matrix.
#' 
#' @importFrom stats as.dist
#' @importFrom methods is
#' 
#' @examples
#' set.seed(42)
#' M <- matrix(rpois(100, lambda=100), ncol=5)
#' colnames(M) <- paste0("sample", 1:5)
#' rownames(M) <- paste0("gene", 1:20)
#' Mnorm <- apply(M, 2, function(x) x/sum(x))
#' Mjsd <- jsd(Mnorm)
#' # equivalently
#' Mjsd <- jsd(M, normalizeCounts=TRUE)
#' Mjsd
#' plot(hclust(Mjsd))
#'   
#' @export
jsd <- function(M, pseudocount=1e-6, normalizeCounts=FALSE) {
    helpmsg <- "M must be a matrix of probabilities between 0 and 1, or if normalizeCounts=TRUE, a matrix of counts >=0."
    if (normalizeCounts) {
        if (!is(M, "matrix") | any(M<0)) stop(helpmsg)
        if (all(M<1)) warning("Are you sure you meant to normalize this? Counts look small...")
        if (typeof(M)!="integer") warning("Are you sure you are trying to normalize a count matrix? It isn't integer...")
        M <- apply(M, 2, function(x) x/sum(x))
    }
    if (!is(M, "matrix") | any(M<0) | any(M>1) ) stop(helpmsg)
    # prevent div/0 and logarithm errors
    M[M==0] <- pseudocount
    # Kullback-Leibler divergence between two vectors
    KLD <- function(x,y) sum(x *log(x/y))
    # Jensen-shannon divergence between two vectors
    JSD <- function(x,y) sqrt(0.5 * KLD(x, (x+y)/2) + 0.5 * KLD(y, (x+y)/2))
    # create and populate output
    out <- matrix(0, ncol(M), ncol(M))
    for(i in 1:ncol(M)) {
        for(j in 1:ncol(M)) { 
            out[i,j]=JSD(as.vector(M[,i]), as.vector(M[,j]))
        }
    }
    colnames(out) <- rownames(out) <- colnames(M)
    out <- as.dist(out)
    attr(out, "method") <- "dist"
    return(out) 
}
