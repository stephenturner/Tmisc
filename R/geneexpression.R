#' Tidy DESeq2 result
#' 
#' Returns a tidy version of a DESeq2 results table.
#' 
#' @param deseqresult Results from running \code{results(dds)} on a DESeqDataSet object.
#' @param colname The name of the column you want to use for what DESeq puts in the row name.
#'   
#' @return a tidy version of the DESeq2 results.
#' 
#' @examples
#' \dontrun{
#' res <- results(dds)
#' res <- deseqresult2tbl
#' }
#'   
#' @export
deseqresult2tbl <- function(deseqresult, colname="ensgene") {
    .Deprecated("results(..., tidy=TRUE)")
    baseMean <- padj <- pvalue <- stat <- NULL #gets rid of the note on pkg check
    if (class(deseqresult) != "DESeqResults") stop("Not a DESeqResults object.")
    deseqresult <- as.data.frame(deseqresult)
    deseqresult %>% 
        as.data.frame() %>% 
        tibble::rownames_to_column(var=colname) %>% 
        tibble::as_tibble() %>% 
        dplyr::arrange(padj, pvalue, dplyr::desc(stat), dplyr::desc(baseMean))
}


#' Fragments per kilobase per million
#' 
#' Takes a count matrix and a vector of gene lengths and returns an optionally \code{log2}-transformed FPKM matrix. Modified from edgeR.
#' 
#' @param x a matrix of counts
#' @param length a vector of length \code{nrow(x)} giving length in bases
#' @param log logical, if \code{TRUE}, then \code{log2} values are returned.
#' @param prior.count average count to be added to each observation to avoid 
#'   taking log of zero. Used only if \code{log=TRUE}.
#'   
#' @return A matrix of FPKM values.
#' 
#' @examples
#' \dontrun{
#' library(readr)
#' library(dplyr)
#' countdata <- read_csv("http://files.figshare.com/2439061/GSE37704_featurecounts.csv") 
#' counts <- countdata %>% select(countdata, starts_with("SRR")) %>% as.matrix
#' counts2fpkm(counts, countdata$length)
#' }
#'   
#' @export
counts2fpkm <- function(x, length, log=FALSE, prior.count=.25) {
    # sanity checks
    if (!methods::is(x, "matrix")) stop("x must be a matrix")
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


#' Rownames to symbol-probeID
#' 
#' This function takes an \code{exprs(eset)} matrix where the rownames are
#' probeset IDs and takes an annotated topTable output where you have an ID and
#' Symbol column and outputs a character vector with symbol_probeid for each
#' probeid in rownames(exprs(eset)). You can use this such that the output on a
#' heatmap contains the gene names concatenated to the probe ID in case you have
#' multiple symbols with the same probeID.
#' 
#' @param exprset The output of \code{exprs(eset)}.
#' @param tt A topTable object.
#'   
#' @return Character vector of the gene symbol with the probe ID.
#' 
#' @examples
#' \dontrun{
#' rownames_to_symprobe(esprs(eset), topTable(fit, number=nrow(fit)))
#' }
#'   
#' @export
rownames_to_symprobe <- function (exprset, tt) {
    .Deprecated()
    if(class(tt)!="data.frame") stop("tt isn't a data frame")
    if(!methods::is(exprset, "matrix")) stop("exprset isn't a matrix")
    if(nrow(tt)!=nrow(exprset)) stop("nrow(tt) != nrow(exprset)")
    tt_index <- sapply(rownames(exprset), function(pattern) grep(pattern, tt$ID))
    newrownames <- paste(tt$Symbol[tt_index], tt$ID[tt_index], sep="_")
    newrownames
}
