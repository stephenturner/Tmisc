#' Jensen-Shannon divergence
#' 
#' Calculates a distance matrix from a matrix of probability distributions using
#' Jensen-Shannon divergence. Adapted from \url{https://enterotype.embl.de/enterotypes.html#dm}.
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


#' Linear model p-value
#' 
#' Extract F-test p-value from a linear model object. Can also use \code{broom::glance(fit)}. Originally described at \url{https://web.archive.org/web/20200829213926/https://gettinggeneticsdone.blogspot.com/2011/01/rstats-function-for-extracting-f-test-p.html}.
#'
#' @param modelobject A model object of class \code{lm}. 
#' @return The p-value on the f-test of a linear model object testing the null hypothesis that R^2==0.
#' 
#' @examples
#' # simulate some (e.g. SNP genotype) data
#' set.seed(42)
#' n=20
#' d=data.frame(x1=rbinom(n,2,.5), x2=rbinom(n,2,.5))
#' d=transform(d, y=x1+x2+rnorm(n))
#' #fit the linear model
#' fit=lm(y ~ x1 + x2, data=d)
#' #shows that the F-test is 0.006641
#' summary(fit)
#' #can't access that p-value using this
#' names(summary(fit)) 
#' # this doesn't work either
#' names(fit)
#  # use the lmp() function:
#' lmp(fit)
#' 
#' @export
lmp <- function (modelobject) {
    if (class(modelobject) != "lm") stop("Not an object of class 'lm'.")
    f <- summary(modelobject)$fstatistic
    # f[1]=value, f[2]=numeratorDF, f[3]=denominatorDF
    p <- stats::pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    return(p)
}


#' Mode.
#' 
#' Returns the mode of a vector. First in a tie wins (see examples).
#' 
#' @param x A vector.
#' @param na.rm Remove missing values before calculating the mode (FALSE by
#'   default). NAs are counted just like any other element. That is, an NA in
#'   the vector won't necessarily result in a return NA. See the first example.
#'   
#' @return A combined p-value.
#'   
#' @examples
#' Mode(c(1,2,2,3,3,3, NA))
#' Mode(c(1,2,2,3,3,3, NA), na.rm=TRUE)
#' Mode(c(1,2,2,3,3,3, NA, NA, NA, NA))
#' Mode(c(1,2,2,3,3,3, NA, NA, NA, NA), na.rm=TRUE)
#' Mode(c("A", "Z", "Z", "B", "B"))
#' 
#' @export
Mode <- function(x, na.rm=FALSE) {
    if (!is.vector(x)) stop("x is not a vector.")
    if(na.rm) x <- x[!is.na(x)]
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}


#' Fisher's method to combine p-values.
#' 
#' Uses Fisher's method to combine p-values from different tests.
#' 
#' @param x A vector of p-values between 0 and 1.
#' @return A combined p-value.
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