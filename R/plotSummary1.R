#' @export plotSummary1
plotSummary1 <- function(x,xlabel=NULL,histogram=TRUE,boxplot=TRUE,
                         rug=TRUE, densityCurve=TRUE, normalCurve=FALSE,
                         addMeanLine=TRUE){
  histColor="#00A4FF"
  if (is.null(xlabel)) xlabel=deparse(substitute(x))
  x=na.omit(x)
  gr <- ggplot(data.frame(x), aes(x=x), geom = 'blank')
  if (histogram){
    gr<- gr+geom_histogram(aes(y=..density..),
                   breaks=seq(min(x),max(x),length=nclass.Sturges(x)),
                   col=histColor, fill=histColor, alpha = .4) + ylab("Frequency") +xlab(xlabel)
    if (addMeanLine) gr<-gr+geom_vline(aes(xintercept=mean(x, na.rm=T)),   # Ignore NA values for mean
                                     color=histColor, linetype="dashed", size=1)
    if (rug) gr<-gr+ geom_rug()
    if (densityCurve)
      gr=gr+geom_line(aes(y = ..density.., colour = 'Empirical'), stat = 'density',size=0.8)
    if (normalCurve)
      gr=gr+stat_function(fun = dnorm, args=list(mean=mean(x), sd=sd(x)), aes(colour = 'Normal'),
                          size=0.8)
    if (normalCurve|densityCurve)
      gr=gr+scale_colour_manual(name = 'Density', values = c('blue', 'red')) +
      theme(legend.position = c(0.85, 0.85))
  }
  if (boxplot){
    bp<-ggplot(data.frame(x), aes(x=factor(""),y=x)) + xlab("") + ylab(xlabel) +
      geom_boxplot(fill=histColor,col=histColor, alpha=.4)+
      stat_summary(fun.y="mean", geom="point", shape=15, size=3, col=histColor)
  }
  if (histogram&boxplot){
    gr <- gr + theme(#legend.position="none",
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      plot.margin=unit(c(0.2,0.2,-0.4,0.2), "cm"))
    bp <- bp + coord_flip()
      theme(plot.margin=unit(c(-0.4,0.2,0.2,0.2), "cm"))
    gp1 <- ggplotGrob(gr)
    gp2 <- ggplotGrob(bp)
    maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3])
    gp1$widths[2:3] <- maxWidth
    gp2$widths[2:3] <- maxWidth
    gr <- grid.arrange(gp1, gp2, ncol=1, nrow=2, widths=c(4), heights=c(4.5, 0.75))
  } else if (boxplot) gr <- bp
  plot(gr)
}
