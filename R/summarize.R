#' @title Univariate summary of a variable for each level of a factor variable (if any is specified)
#' @description summarize makes a simple description of a variable in one or more groups.
#' It first tests if the variable has a normal distribution by a Shapiro-Wilk test. In the
#' normal case the variable is summarized in mean and sd in each group. In other case it is
#' summarized in median and percentiles (by default 25 and 75 percentiles). A comparison
#' betweed two groups is made by a t.test in the normal case and by a wilcoxon test in other case.
#' When there are more than two groups, anova F-test and Kruskal-Wallis test are used.
#' Optionally histograms and boxplots are printed.
#' @export summarize
#' @param x variable to be summarized.
#' @param by variable for identifying groups, interpreted as factor
#' @param bylabel Label of the variable by, to be used in figures and tables.
#' @param xlabel Label of the variable x, to be used in figures and tables.
#' @param plot Logical. If TRUE a plot is printed
#' @param ptiles ptiles to print with the median when variable is not normal.
#' @param alphaNorm Significance level for testing normality. 0.05 by default.
#' @param report. If "auto" a Shapiro test is made; only mean and sd (if variable is accepted to be
# normal) or median and quartiles (when variable is not normal) are shown. If "meansd", only mean and sd
# are shown; if "medianq", median and percentiles are shown. In other case a full report is printed,
# including also number of missing values, skewness, kurtosis, min, max and the p-value of the Shapiro
# test for normality
#' @param histogram. Should a histogram be drawn for each level of the factor variable?
#' @param boxplot Should a boxplot be added under the histograms?
#' @param normalCurve Logical. Must a normal curve be superposed to the histograms?
#' @param densityCurve Logical. Must a density curve be superposed to the histograms?
#' @param rug must a rug be drawn at the base of the histograms?
#' @param faceted. When FALSE histograms are superposed. When TRUE are shown separately
#' @param addMeanLine. Should a vertical lines be added to the histogram at the position of the mean?
#' @return
#'
summarize=function(x,by=NULL,xlabel=NULL,bylabel=NULL, plot=TRUE, ptiles=c(0.25,0.75), alphaNorm=0.05,
                  report="auto",histogram=TRUE,boxplot=TRUE, rug=TRUE,faceted=TRUE,
                  densityCurve=TRUE, normalCurve=FALSE, addMeanLine=TRUE,digits=2,showSummary=TRUE){
  if (is.null(xlabel)) xlabel=toLabel(deparse(substitute(x)))
  if (is.null(by)){
    summary1(x=x,xlabel=xlabel, plot=plot, ptiles=ptiles, alphaNorm=alphaNorm,
             report=report,histogram=histogram,boxplot=boxplot,rug=rug,
             densityCurve=densityCurve,normalCurve=normalCurve,addMeanLine=addMeanLine,
             showSummary=showSummary)
  } else{
    if (is.null(bylabel)) bylabel=toLabel(deparse(substitute(by)))
    if (report%in%c("auto","meansd","medianq")) fullReport=FALSE else fullReport=TRUE
    summary2(x=x,by=by,xlabel=xlabel,bylabel=bylabel, plot=plot, ptiles=ptiles, alphaNorm=alphaNorm,
             fullReport=fullReport,histogram=histogram,boxplot=boxplot,rug=rug,faceted=faceted,
             densityCurve=densityCurve,normalCurve=normalCurve,addMeanLine=addMeanLine,digits=digits,
             showSummary=showSummary)
  }
}
