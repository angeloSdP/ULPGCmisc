#' @title Univariate summary
#' @description summary1 makes a simple description of a variable. It first tests if
#' variable has a normal distribution by a Shapiro-Wilk test. In the normal case the
#' variable is summarized in mean and sd. In other case it is summarized in median
#' and percentiles (by default 25 and 75 percentiles). Optionally an histogram is
#' printed.
#' @export summary1
#' @param x variable to be summarized.
#' @param xlabel Label of the variable x, to be used in figures and tables.
#' @param plot Logical. If TRUE a barplot is printed
#' @param ptiles ptiles to print with the median when variable is not normal.
#' @param alphaNorm Significance level for testing normality. 0.05 by default.
#' @param fullreport. If FALSE only mean and sd (when data are normally distributed) or
#' median and quartiles (when data are not normal) are shown. In other case a full report is printed,
#' including also number of missing values, skewness, kurtosis, min, max and the p-value of the Shapiro
#' test for normality
#' @param normalCurve Logical. Must a normal curve be superposed to the histogram?
#' @param densityCurve Logical. Must a density curve be superposed to the histogram?
#' @param rug must a rug be drawn at the base of the histogram?
#' @param boxplot Should a boxplot be added under the histogram?
#' @param addMeanLine Should a vertical line be drawn at the position of the mean?
#' @param histColor color for plotting the histogram.
#' @return a brief summary of a continuous variable and optionally an histogram.
#'
summary1=function(x,xlabel=NULL, plot=TRUE, ptiles=c(0.25,0.75), alphaNorm=0.05,
                  fullreport=FALSE,histogram=TRUE,boxplot=TRUE,rug=TRUE,
                  densityCurve=TRUE,normalCurve=FALSE,addMeanLine=TRUE,showSummary=TRUE){
  panderOptions('knitr.auto.asis', FALSE)
  panderOptions('keep.line.breaks', TRUE)
  panderOptions('digits',4)
  if (is.null(xlabel)) xlabel=deparse(substitute(x))
  shpv=shap_pval(x)
  normal=if (is.na(shpv)) NA else shpv>=alphaNorm
  if (!fullreport){
    rsname=if (normal|is.na(normal)) "mean (sd)" else
      paste("median (",paste(paste("q",100*ptiles,sep=""),collapse="-"),")",sep="")
    if (is.na(normal)) resumen=data.frame(xlabel,"NA (NA)")
    else if (normal) resumen=data.frame(xlabel, meansd(x))
    else resumen=data.frame(xlabel,medianPtiles(x,ptiles))
    names(resumen)=c("Variable",rsname)
    if (showSummary) pander(resumen)
  } else{
    resumen=fullReport(x,xlabel)
    resumen$shapiro.test.Pvalue=formatPval(shpv)
    if (showSummary) pander(resumen,split.table=90)
  }
  if (plot&!is.na(normal))
    plotSummary1(x,xlabel,histogram,boxplot,rug,densityCurve,normalCurve,addMeanLine)
  return(invisible(resumen))
}
