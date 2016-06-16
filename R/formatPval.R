#' @title Pvalue formatting
#' @description formatPval formats p-values. If pvalue is less than 0.0001 it is shown
#' as the string "<0.0001". Else it is rounded to 4 decimals.
#' @export formatPval
#' @param pval pvalue to be formatted
#' @return formatted p-value
#'
formatPval=function(pval){
  if (is.na(pval)) NA
  else if (pval<0.0001) "<0.0001"
  else as.character(sprintf("%5.4f",pval))
}
