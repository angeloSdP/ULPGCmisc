# @title Univariate summary
# @description summary1 makes a simple description of a variable. It first tests if
# variable has a normal distribution by a Shapiro-Wilk test. In the normal case the
# variable is summarized in mean and sd. In other case it is summarized in median
# and percentiles (by default 25 and 75 percentiles). Optionally an histogram is
# printed.
# @param x variable to be summarized.
# @param xlabel Label of the variable x, to be used in figures and tables.
# @param plot Logical. If TRUE a barplot is printed
# @param ptiles ptiles to print with the median when variable is not normal.
# @param alphaNorm Significance level for testing normality. 0.05 by default.
# @param report. If "auto" a Shapiro test is made; only mean and sd (if variable can be assumed to be
# normal) or median and quartiles (when variable is not normal) are shown. If "meansd", only mean and sd
# are shown; if "medianq", median and percentiles are shown. In other case a full univariate report is printed,
# including also number of missing values, skewness, kurtosis, min, max and the p-value of the Shapiro
# test for normality.
# @param normalCurve Logical. Must a normal curve be superposed to the histogram?
# @param densityCurve Logical. Must a density curve be superposed to the histogram?
# @param rug must a rug be drawn at the base of the histogram?
# @param boxplot Should a boxplot be added under the histogram?
# @param addMeanLine Should a vertical line be drawn at the position of the mean?
# @param histColor color for plotting the histogram.
# @return a brief summary of a continuous variable and optionally an histogram.
#
summary1=function(x,xlabel=NULL, plot=TRUE, ptiles=c(0.25,0.75), alphaNorm=0.05,
                  report="auto",histogram=TRUE,boxplot=TRUE,rug=TRUE,
                  densityCurve=TRUE,normalCurve=FALSE,addMeanLine=TRUE,showSummary=TRUE){
  panderOptions('knitr.auto.asis', FALSE)
  panderOptions('keep.line.breaks', TRUE)
  panderOptions('digits',4)
  if (is.null(xlabel)) xlabel=toLabel(deparse(substitute(x)))
  x=na.omit(x)
  n=length(which(!is.na(x)))
  if (report=="meansd"){
    shpv=shap_pval(x)
    normal=if (is.na(shpv)) TRUE else shpv>=alphaNorm
    if (!normal)
      warning(paste("Variable is not normally distributed (Shapiro test P",
                    formatPval(shpv),
                    "). \n Maybe description with mean and sd can be not adequate.", sep=""),
              call.=FALSE)
  }
  if (report=="auto"){
    shpv=shap_pval(x)
    normal=if (is.na(shpv)) TRUE else shpv>=alphaNorm
    report=if (normal) "meansd" else "medianq"
  }
  if (report=="meansd"){
    rsname="mean (sd)"
    resumen=data.frame(xlabel, meansd(x))
    names(resumen)=c("Variable",paste("All data\n(n=",n,")",sep=""))
    if (showSummary) pander(resumen, caption=paste("Data are summarized as",rsname),
                            keep.line.breaks=TRUE)
  } else if (report=="medianq"){
    rsname=paste("median (",paste(paste("q",100*ptiles,sep=""),collapse="-"),")",sep="")
    resumen=data.frame(xlabel,medianPtiles(x,ptiles))
    names(resumen)=c("Variable",paste("All data\n(n=",n,")",sep=""))
    if (showSummary) pander(resumen, caption=paste("Data are summarized as",rsname),
                            keep.line.breaks=TRUE)
  } else{
    resumen=univariateReport(x,xlabel)
    shpv=shap_pval(x)
    resumen$shapiro.test.Pvalue=formatPval(shpv)
    if (showSummary) pander(resumen,split.table=90)
  }
  if (plot&n>0)
    plotSummary1(x,xlabel,histogram,boxplot,rug,densityCurve,normalCurve,addMeanLine)
  return(invisible(resumen))
}
