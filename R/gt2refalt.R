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
