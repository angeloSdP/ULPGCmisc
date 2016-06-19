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
                  fullreport=FALSE,normalCurve=FALSE,densityCurve=TRUE,rug=TRUE,
                  boxplot=TRUE, addMeanLine=TRUE,histColor="#00A4FF"){
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
    pander(resumen)
  } else{
    resumen=fullReport(x,xlabel)
    resumen$shapiro.test.Pvalue=formatPval(shpv)
    pander(resumen,split.table=90)
  }
  if (plot&!is.na(normal)){
    x=x[!is.na(x)]
    gr <- qplot(x, geom = 'blank') +
      geom_histogram(aes(y=..density..),
                     breaks=seq(min(x),max(x),length=nclass.Sturges(x)),
                     col=histColor, fill=histColor, alpha = .4) + ylab("Frequency")
    if (addMeanLine) gr<-gr+geom_vline(aes(xintercept=mean(x, na.rm=T)),   # Ignore NA values for mean
                      color=histColor, linetype="dashed", size=1)
    if (rug) gr<-gr+ geom_rug()
    if (densityCurve)
      gr=gr+geom_line(aes(y = ..density.., colour = 'Empirical'), stat = 'density',size=0.8)
    if (normalCurve)
      gr=gr+stat_function(fun = dnorm, args=list(mean=mean(x), sd=sd(x)), aes(colour = 'Normal'),size=0.8)
    if (normalCurve|densityCurve)
      gr=gr+scale_colour_manual(name = 'Density', values = c('blue', 'red')) +
      theme(legend.position = c(0.85, 0.85))
    if (boxplot){
      gr <- gr + theme(#legend.position="none",
                       axis.text.x=element_blank(),
                       axis.ticks.x=element_blank(),
                       plot.margin=unit(c(0.2,0.2,-0.4,0.2), "cm"))
      bp <- ggplot(data.frame(x), aes(x=factor(""),y=x)) + geom_boxplot(fill=histColor,col=histColor, alpha=.4)+
        coord_flip() + xlab("") + ylab(xlabel) +
        stat_summary(fun.y=mean, geom="point", shape=15, size=3, col=histColor)+
        theme(plot.margin=unit(c(-0.4,0.2,0.2,0.2), "cm"))
      p1 <- ggplot_gtable(ggplot_build(gr))
      p2 <- ggplot_gtable(ggplot_build(bp))
      maxWidth = unit.pmax(p1$widths[2:3], p2$widths[2:3])
      p1$widths[2:3] <- maxWidth
      p2$widths[2:3] <- maxWidth
      gr<-grid.arrange(p1, p2, ncol=1, nrow=2, widths=c(4), heights=c(4.5, 0.75))
    } else gr <- gr + xlab(xlabel)
    plot(gr)
  }
  return(invisible(resumen))
}
