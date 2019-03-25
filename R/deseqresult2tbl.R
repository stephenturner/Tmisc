#' Tidy DESeq2 result
#' 
#' Returns a tidy version of a DESeq2 results table.
#' 
#' @author Stephen Turner
#' @keywords NA
#' 
#' @param deseqresult Results from running \code{results(dds)} on a DESeqDataSet object.
#' @param colname The name of the column you want to use for what DESeq puts in the row name.
#'   
#' @return a tidy version of the DESeq2 results.
#' 
#' @import dplyr
#' @importFrom tibble as_tibble rownames_to_column
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
    gene <- baseMean <- padj <- pvalue <- stat <- NULL #gets rid of the note on pkg check
    if (class(deseqresult) != "DESeqResults") stop("Not a DESeqResults object.")
    deseqresult <- as.data.frame(deseqresult)
    deseqresult %>% 
        as.data.frame() %>% 
        rownames_to_column(var=colname) %>% 
        as_tibble() %>% 
        arrange(padj, pvalue, desc(stat), desc(baseMean))
}
