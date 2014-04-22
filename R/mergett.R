#' Merge topTable with expression values
#' 
#' Merges topTable output with normalized expression values from the
#' ExpressionSet. Note: run \code{addRawFC(tt)} before merging with
#' ExpressionSet values.
#' 
#' @param tt A topTable
#' @param eset An ExpressionSet object from which tt derives
#'   
#' @return An expanded topTable with gene expression values merged in, sorted by p-value.
#'   
#' @keywords keywords
#'   
#' @export
#' 
#' @examples
#' # fit <- lmFit(eset,design)
#' # fit <- contrasts.fit(fit, contrast.matrix)
#' # fit <- eBayes(fit)
#' # tt <- topTable(fit, coef="mycontrast", number=nrow(fit))
#' # tt <- addRawFC(tt)
#' # tt <- mergett(tt, eset)

mergett <- function(tt, eset) {
    require(Biobase)
    if (class(tt)!="data.frame") stop("tt should be a data.frame")
    if (class(eset)!="ExpressionSet") stop("eset should be an ExpressionSet")
    exprset <- data.frame(exprs(eset))
    exprset$ID <- rownames(exprset)
    rownames(exprset) <- NULL
    message(paste("nrow(tt):", nrow(tt)))
    message(paste("nrow(exprset):", nrow(exprset)))
    if (nrow(tt)==nrow(exprset)) {
        # Merge with intensities
        newtt <- merge(tt, exprset)
    } else {
        stop("nrow(tt) != nrow(exprset)")
    }
    message(paste("nrow(newtt):", nrow(newtt)))
    newtt <- newtt[order(newtt$P.Val), ] #reorder by p-value
    rownames(newtt) <- NULL
    newtt
}