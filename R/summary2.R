#' @title Univariate summary of a variable for each level of a factor variable.
#' @description summary2 makes a simple description of a variable in two or more groups.
#' It first tests if the variable has a normal distribution by a Shapiro-Wilk test. In the
#' normal case the variable is summarized in mean and sd in each group. In other case it is
#' summarized in median and percentiles (by default 25 and 75 percentiles). A comparison
#' betweed groups is made by a t.test in the normal case and by a wilcoxon test in other case.
#' Optionally histograms and boxplots are printed.
#' @export summary2
#' @param x variable to be summarized.
#' @param by variable for identifying groups, interpreted as factor
#' @param bylabel Label of the variable by, to be used in figures and tables.
#' @param xlabel Label of the variable x, to be used in figures and tables.
#' @param plot Logical. If TRUE a plot is printed
#' @param ptiles ptiles to print with the median when variable is not normal.
#' @param alphaNorm Significance level for testing normality. 0.05 by default.
#' @param fullreport. If FALSE only mean and sd (when data are normally distributed) or
#' median and quartiles (when data are not normal) are shown. In other case a full report is printed,
#' including also number of missing values, skewness, kurtosis, min, max and the p-value of the Shapiro
#' test for normality
#' @param normalCurve Logical. Must a normal curve be superposed to the histograms?
#' @param densityCurve Logical. Must a density curve be superposed to the histograms?
#' @param rug must a rug be drawn at the base of the histograms?
#' @param boxplot Should a boxplot be added under the histograms?
#' @param histColor colors for plotting the histograms.
#' @return
#'
summary2=function(x,by,xlabel=NULL,bylabel=NULL, plot=TRUE, ptiles=c(0.25,0.75), alphaNorm=0.05,
                  fullreport=FALSE,boxplot=TRUE,histogram=TRUE, rug=TRUE,faceted=TRUE,
                  densityCurve=TRUE, normalCurve=FALSE, addMeanLine=TRUE,digits=2,
                  showSummary=TRUE){
  if (is.null(xlabel)) xlabel=deparse(substitute(x))
  if (is.null(bylabel)) bylabel=deparse(substitute(by))
  if (is.numeric(by)&length(unique(by))>8){
    breaks <- pretty(by, n = min(nclass.Sturges(by),6), min.n = 1)
    lb=length(breaks)
    if (lb>8) breaks <- pretty(by, n =lb%/%2, min.n = 1)
    by=cut(by,breaks=breaks)
  }
  by=factor(by)
  shapiro_pvs=tapply(x,by,shap_pval)
  if (any(is.na(shapiro_pvs))) shapiro_pvs[is.na(shapiro_pvs)]=1
  normal=all(shapiro_pvs>=alphaNorm)
  nl=length(levels(by))
  if (normal){
    msd=meansd(x)
    rsname="mean (sd)"
    resum1=tapply(x, by, meansd)
    if (nl<2) pval=""
    else if (nl==2){
      pval=tryCatch(formatPval(t.test(x~by)$p.value), error=function(e) "NaN")
      test="t-test"
    } else if (nl>2){
      pval=tryCatch(formatPval(anova(lm(x~by))$"Pr(>F)"[1]), error=function(e) "NaN")
      test="anova F-test"
    }
  }
  else{
    msd=medianPtiles(x,ptiles)
    rsname=paste("median (",paste(paste("q",100*ptiles,sep=""),collapse="-"),")",sep="")
    resum1=tapply(x,by,medianPtiles,ptiles)
    if (nl<2) pval=""
    else if (nl==2){
      pval=tryCatch(formatPval(wilcox.test(x~by)$p.value), error=function(e) "NaN")
      test="Wilcoxon test"
    }
    else if (nl>2){
      pval=tryCatch(formatPval(kruskal.test(x~by)$p.value), error=function(e) "NaN")
      test="Kruskal test"
    }
  }
  if (!fullreport){
    resumen=data.frame(xlabel,msd,matrix(resum1,nrow=1),pval)
    nmby=paste(bylabel,"=",levels(by),sep="")
    names(resumen)=c("Variable",rsname,nmby,paste(test,"P"))
  } else {
    resumen=do.call(rbind,tapply(x,by,fullReport))
    resumen$Variable=rownames(resumen)
    rownames(resumen)=NULL
    resumen$shapiro.test.Pvalue=sapply(shapiro_pvs,formatPval)
    resumen[,-c(1,ncol(resumen))]=apply(resumen[,-c(1,ncol(resumen))],2,round,digits)
    names(resumen)[1]="Value"
    pw=rep("",nrow(resumen)-1)
    resumen=cbind(Variable=c(xlabel,pw),resumen,P=c(pw,pval), test=c(pw,paste("(",test,")",sep="")))
    names(resumen)[ncol(resumen)]="Comparison test"
    resumen
  }
  if (showSummary) pander(resumen,split.table=90)
  if (plot) plotSummary2(x,by,xlabel,bylabel,boxplot,histogram,rug,faceted,
                         densityCurve, normalCurve,addMeanLine)
  return(invisible(resumen))
}
