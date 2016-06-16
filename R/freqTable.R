#' @title Univariate and bivariate frequency tables and barplots
#' @description freqTable builds an univariate or bivariate frequency table with
#' counts and percentages at each combination of factor levels factor level and
#' optionally shows a barplot.
#' @export freqTable
#' @param x variable to tabulate, which can be interpreted as factor
#' @param by variable for crosstabulation, also interpreted as factor
#' @param xlabel Label of the variable x, to be used in figures and tables.
#' @param bylabel Label of the variable by, to be used in figures and tables.
#' @param plot Logical. If TRUE a barplot is printed
#' @param horizontal Logical. If TRUE bars are printed horizontally
#' @param printFreq Logical. If TRUE frequency values are printed in the barplot
#' @param density Logical. if TRUE barplot is printed with relative frequencies
#' @param showTable Logical. If TRUE frequency table is printed via pander. If FALSE
#' frequency table is returned invisibly.
#' @return an univariate frequency table with absolute and relative frequencies, and
#' optionally a barplot
#' @examples
#' symptom=sample(LETTERS[1:4],120,replace=TRUE)
#' disease=sample(c("Yes","No"),120,replace=TRUE)
#' freqTable(symptom,by=disease,horizontal=FALSE,density=TRUE,printFreq=TRUE,showTable=TRUE)
#' freqTable(symptom,horizontal=FALSE,density=TRUE,printFreq=TRUE,showTable=FALSE)
#'
freqTable=function(x,by=NULL,xlabel=NULL,bylabel=NULL,plot=TRUE,horizontal=FALSE,
                   printFreq=TRUE, density=TRUE, showTable=TRUE){
  if (is.null(xlabel)) xlabel=deparse(substitute(x))
  if (is.null(by)){
    table1(x=x,xlabel=xlabel,plot=plot,horizontal=horizontal,
           printFreq=printFreq,density=density, showTable=showTable)
  } else{
    if (is.null(bylabel)) bylabel=deparse(substitute(by))
    table2(x=x,by=by,xlabel=xlabel,bylabel=bylabel,plot=plot,horizontal=horizontal,
               printFreq=printFreq,density=density, showTable=showTable)
  }
}
