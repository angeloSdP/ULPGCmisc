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
#' @param pctBycol Logical. If TRUE, relative frequencies in crosstabs are computed by columns else by rows.
#' @param report. If "auto" a Shapiro test is made; only mean and sd (if variable can be assumed to be
# normal) or median and quartiles (when variable is not normal) are shown. If "meansd", only mean and sd
# are shown; if "medianq", median and percentiles are shown. In other case a full report is printed,
# including also number of missing values, skewness, kurtosis, min, max and the p-value of the Shapiro
# test for normality.
#' @return
#'
describe=function(x,by=NULL,xlabel=NULL,bylabel=NULL, plot=FALSE,
                  report="auto", showDescriptives=TRUE,pctBycol=TRUE){
  desc=function(x,by=NULL,xlabel=xlabel,bylabel=bylabel, plot=plot,
                    report=report, showDescriptives=TRUE){
    if (is.factor(x)|is.character(x))
      freqTable(x=x,by=by, xlabel=xlabel,bylabel=bylabel,plot=plot,
                showTable = showDescriptives, pctBycol=pctBycol)
    else summarize(x=x,by=by, xlabel=xlabel,bylabel=bylabel,plot=plot,
                   report=report,showSummary=showDescriptives)
  }
  panderOptions('knitr.auto.asis', FALSE)
  panderOptions('keep.line.breaks', TRUE)
  panderOptions('table.style',"multiline")
  dsby=deparse(substitute(by))
  if ((dsby!="NULL")& is.null(by)) stop(paste("Variable",dsby,"not found"))
  if (!is.null(by)&is.null(bylabel)) bylabel=toLabel(dsby)
  if (is.null(xlabel)){
    if (is.data.frame(x)) xlabel=names(x) else
        xlabel=toLabel(deparse(substitute(x)))
  } else
    if (is.data.frame(x)&!is.null(xlabel)&length(unique(xlabel))<NCOL(x)){
      txl=data.frame(table(xlabel))
      reps=which(txl$Freq>1)
      for (r in reps){
        wr=which(xlabel==txl[r,1])
        xlabel[wr]=paste(txl[r,1],1:txl[r,2],sep=".")
      }
    }
  if (is.data.frame(x)){
    if (length(report)<ncol(x)) report=rep(report[1],ncol(x))
    vv=validValues(x, by=by, byname=toLabel(dsby))
    NApresent=vv$haveNA
    resumen=NULL
    nms=names(vv$nValid)
    if (length(nms)==5) nms[5]="P" else if (length(nms)==3) nms=nms[-3]
    if (NApresent) nms=sapply(strsplit(nms,"\n"), function(x) x[1])
    for (j in 1:ncol(x)){
      rj=desc(x=x[,j],by=by,xlabel=xlabel[j],bylabel=bylabel, plot=plot,
              report=report[j],showDescriptives = FALSE)
      resumen=rbind(resumen,setNames(rj,nms))
    }
    if (showDescriptives) pander(resumen,split.table=Inf)
    if(NApresent){
      warning("Missing values are present. Not all the variables are evaluated on the same sample size.",
                                  call.=FALSE)
      return(invisible(list(summary=resumen, nValid=vv$nValid)))
    } else return(invisible(list(summary=resumen)))
  } else{
    resumen=desc(x,by=by,xlabel=xlabel,bylabel=bylabel, plot=plot,
                 report=report, showDescriptives=showDescriptives)
    return(invisible(list(summary=resumen)))
  }
}
