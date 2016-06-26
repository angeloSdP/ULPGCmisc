#' @title Univariate summary of a variable for each level of a factor variable (if any is specified)
#' @description describe makes a simple description of a variable 'x' in one or more groups defined
#' by the factor 'by'. If 'x' is a factor, frequency tables and (optionally) barplots are shown. If 'x'
#' is a numeric variable, the variable is summarized in mean and sd in each group if 'x' is normal.
#' In other case it is summarized in median and percentiles (by default 25 and 75 percentiles). A comparison
#' betweed two groups is made by a t.test in the normal case and by a wilcoxon test in other case.
#' When there are more than two groups, anova F-test and Kruskal-Wallis test are used.
#' Optionally histograms and boxplots are printed.
#' @export describe
#' @param x variable to be summarized.
#' @param by variable for identifying groups, interpreted as factor
#' @param bylabel Label of the variable by, to be used in figures and tables.
#' @param xlabel Label of the variable x, to be used in figures and tables.
#' @param plot Logical. If TRUE a plot is printed
#' @param report. If "auto" a Shapiro test is made; only mean and sd (if variable can be assumed to be
# normal) or median and quartiles (when variable is not normal) are shown. If "meansd", only mean and sd
# are shown; if "medianq", median and percentiles are shown. In other case a full report is printed,
# including also number of missing values, skewness, kurtosis, min, max and the p-value of the Shapiro
# test for normality.
#' @return
#'
describe=function(x,by=NULL,xlabel=NULL,bylabel=NULL, plot=TRUE,
                  report="auto", showDescriptives=TRUE){
  if (is.null(xlabel)) xlabel=toLabel(deparse(substitute(x)))
  if (is.null(bylabel)) bylabel=toLabel(deparse(substitute(by)))
  if (is.factor(x)|is.character(x))
    freqTable(x=x,by=by, xlabel=xlabel,bylabel=bylabel,plot=plot,
              showTable = showDescriptives)
  else summarize(x=x,by=by, xlabel=xlabel,bylabel=bylabel,plot=plot,
                 report=report,showSummary=showDescriptives)
}
