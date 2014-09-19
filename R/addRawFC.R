#' Add more FCs to topTable
#' 
#' Takes a limma topTable as input, and adds the absolute value of the log2 FC,
#' and the raw FC (2^logFC).
#' 
#' @author Stephen Turner
#' @keywords keywords
#' 
#' @param tt A topTable
#'   
#' @return A new topTable with absLogFC and rawFC
#'   
#' @export
#' 
#' @examples
#' \dontrun{
#' fit <- lmFit(eset,design)
#' fit <- contrasts.fit(fit, contrast.matrix)
#' fit <- eBayes(fit)
#' tt <- topTable(fit, coef="mycontrast", number=nrow(fit))
#' tt <- addRawFC(tt)
#' }
 
addRawFC <- function(tt) {
    if (class(tt)!="data.frame") stop("Doesn't look like a data.frame")
    if (!all(c("logFC", "AveExpr", "t", "P.Value", "adj.P.Val", "B") == names(tt)[(ncol(tt)-5):ncol(tt)])) stop("Doesn't look like a topTable")
    tt <- tt[order(tt$P.Value), ]
    AveExpr=P.Value=adj.P.Val=NULL
    ttmeta <- tt[ ,1:(ncol(tt)-6)]
    logFC <- tt$logFC
    absLogFC <- abs(logFC)
    rawFC <- 2^logFC
    tt2 <- cbind(ttmeta,
                 logFC,
                 absLogFC,
                 rawFC,
                 subset(tt, select=c(AveExpr, t, P.Value, adj.P.Val)))
    if (all(tt$logFC==tt2$logFC)) {
        return(tt2)
    }   else {
        stop("Something went horribly wrong. Check that tt$LogFC==tt2$LogFC")
    }
}