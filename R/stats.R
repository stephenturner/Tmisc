#' Jensen-Shannon divergence
#' 
#' Calculates a distance matrix from a matrix of probability distributions using
#' Jensen-Shannon divergence. Adapted from <https://enterotype.embl.de/>.
#' 
#' @references <https://web.archive.org/web/20240131141033/https://enterotype.embl.de/enterotypes.html#dm>.
#' 
#' @param M a probability distribution matrix, e.g., normalized transcript compatibility counts.
#' @param pseudocount a small number to avoid division by zero errors.
#' @param normalizeCounts logical, whether to attempt to normalize by dividing by the column sums. Set to \code{TRUE} if this is, e.g., a count matrix.
#'   
#' @return A Jensen-Shannon divergence-based distance matrix.
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
        if (!methods::is(M, "matrix") | any(M<0)) stop(helpmsg)
        if (all(M<1)) warning("Are you sure you meant to normalize this? Counts look small...")
        if (typeof(M)!="integer") warning("Are you sure you are trying to normalize a count matrix? It isn't integer...")
        M <- apply(M, 2, function(x) x/sum(x))
    }
    if (!methods::is(M, "matrix") | any(M<0) | any(M>1) ) stop(helpmsg)
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
    out <- stats::as.dist(out)
    attr(out, "method") <- "dist"
    return(out) 
}


#' Mode
#' 
#' Returns the mode of a vector. First in a tie wins (see examples).
#' 
#' @param x A vector.
#' @param na.rm Remove missing values before calculating the mode (FALSE by
#'   default). NAs are counted just like any other element. That is, an NA in
#'   the vector won't necessarily result in a return NA. See the first example.
#'   
#' @return The mode of the input vector.
#' @export
#'   
#' @examples
#' Mode(c(1,2,2,3,3,3, NA))
#' Mode(c(1,2,2,3,3,3, NA), na.rm=TRUE)
#' Mode(c(1,2,2,3,3,3, NA, NA, NA, NA))
#' Mode(c(1,2,2,3,3,3, NA, NA, NA, NA), na.rm=TRUE)
#' Mode(c("A", "Z", "Z", "B", "B"))
#' 
Mode <- function(x, na.rm=FALSE) {
    if (!is.vector(x)) stop("x is not a vector.")
    if(na.rm) x <- x[!is.na(x)]
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}


#' Fisher's method to combine p-values
#' 
#' Uses Fisher's method to combine p-values from different tests.
#' 
#' @param x A vector of p-values between 0 and 1.
#' 
#' @references Fisher, R.A. (1925). Statistical Methods for Research Workers.
#' @references <https://en.wikipedia.org/wiki/Fisher%27s_method>.
#' 
#' @return The combined p-value.
#'   
#' @examples
#' fisherp(c(.042, .02, .001, 0.01, .89))
#' 
#' @export
fisherp <- function(x) {
    if (any(is.na(x))) {
        warning("Some p-values missing; removing these.")
        x <- stats::na.omit(x)
    }
    if (any(x<=0 | x>1)) stop("P-values must be >0 and <=1.")
    if (length(x)<2) stop("Must have at least two valid p-values.")
    df <- 2*length(x)
    fisherp <- stats::pchisq( -2*sum(log(x)), df, lower.tail=FALSE)
    return(fisherp)
}


#' Fragments per kilobase per million
#' 
#' Takes a count matrix and a vector of gene lengths and returns an optionally \code{log2}-transformed FPKM matrix. Modified from edgeR.
#' 
#' @param x a matrix of counts.
#' @param length a vector of length \code{nrow(x)} giving length in bases.
#' @param log logical, if \code{TRUE}, then \code{log2} values are returned.
#' @param prior.count average count to be added to each observation to avoid 
#'   taking log of zero. Used only if \code{log=TRUE}.
#'   
#' @return A matrix of FPKM values.
#' @export
#' 
#' @examples
#' set.seed(123)
#' genecounts <- matrix(sample(c(rep(0, 50), 1:100), 30), nrow=10)
#' lengths <- sample(1000:10000, 10)
#' counts2fpkm(genecounts, lengths)
#'   
counts2fpkm <- function(x, length, log=FALSE, prior.count=.25) {
    # sanity checks
    if (!inherits(x, "matrix")) stop("x must be a matrix")
    if (nrow(x)!=length(length)) stop("dimensions of count matrix and gene lengths don't match")
    
    # library size is sum of reads in each sample
    lib.size <- colSums(x)
    if (log) {
        # If you're log scaling, you'll have to add something to the zeros, so
        # adjust the library sizes accordingly. prior.count is the average count to
        # be added to each observation to avoid taking log of zero.
        prior.count.scaled <- lib.size/mean(lib.size) * prior.count
        lib.size <- lib.size + 2 * prior.count.scaled
    }
    
    # Per million
    lib.size <- 1e-06 * lib.size
    if (log) {
        cpm <- log2(t((t(x) + prior.count.scaled)/lib.size))
    } else {
        cpm <- t(t(x)/lib.size)
    }
    
    # per kilobase
    length.kb <- length/1000
    if (log) {
        fpkm <- cpm-log2(length.kb)
    } else {
        fpkm <- cpm/length.kb
    }
    
    return(fpkm)
}
