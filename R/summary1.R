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
#' @param normalCurve Logical. Must a normal curve be superposed to the histogram?.
#' @return a brief summary of a continuous variable and optionally an histogram.
#'
summary1=function(x,xlabel=NULL, plot=TRUE, ptiles=c(0.25,0.75), alphaNorm=0.05,
                  normalCurve=FALSE){
  panderOptions('knitr.auto.asis', FALSE)
  panderOptions('keep.line.breaks', TRUE)
  if (is.null(xlabel)) xlabel=deparse(substitute(x))
  is.normal=function(x){
    if (length(x)>5000) x=sample(x,5000)
    tryCatch(shapiro.test(x)$p.value, error=function(e) NA)>=alphaNorm
  }
  meansd=function(x) sprintf("%1.2f (%1.2f)", mean(x,na.rm=TRUE), sd(x,na.rm=TRUE))
  medianPtiles=function(x) {
    sprintf("%1.1f (%1.1f;%1.1f)", median(x,na.rm=TRUE),
            quantile(x,na.rm=TRUE,probs=ptiles[1]),
            quantile(x,na.rm=TRUE,probs=ptiles[2]))
  }
  normal=is.normal(x)
  rsname=if (normal) "mean (sd)" else "median (quartiles)"
  if (is.na(normal)) resumen=data.frame(xlabel,"NA (NA)")
  else if (normal) resumen=data.frame(xlabel, meansd(x))
  else resumen=data.frame(xlabel,medianPtiles(x))
  names(resumen)=c("Variable",rsname)
  pander(resumen)
  if (plot&!is.na(normal)){
    gr<- ggplot(data=data.frame(x), aes(x)) +
      geom_histogram(aes(y=..density..),
                     breaks=seq(min(x,na.rm=TRUE),max(x,na.rm=TRUE),length=nclass.Sturges(x)),
                     col="blue", fill="#00A4FF", alpha = .6) + xlab(xlabel) + ylab("Frecuencia")
    if (normalCurve) gr=gr+stat_function(fun = dnorm, args=list(mean=mean(x), sd=sd(x)),colour = "red")
    print(gr)
  }
}
