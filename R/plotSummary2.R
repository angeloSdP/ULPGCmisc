#' @export plotSummary2
plotSummary2 <- function(x,by,xlabel=NULL,bylabel=NULL,boxplot=TRUE,histogram=TRUE,
                         rug=TRUE, faceted=FALSE, densityCurve=FALSE, normalCurve=FALSE,
                         addMeanLine=TRUE){
  AlignPlots <- function(...) {
    LegendWidth <- function(x) x$grobs[[8]]$grobs[[1]]$widths[[4]]
    plots.grobs <- lapply(list(...), ggplotGrob)
    max.widths <- do.call(unit.pmax, lapply(plots.grobs, "[[", "widths"))
    plots.grobs.eq.widths <- lapply(plots.grobs,
                                    function(x) {
                                      x$widths <- max.widths
                                      x
                                    })
    legends.widths <- lapply(plots.grobs, LegendWidth)
    max.legends.width <- do.call(max, legends.widths)
    plots.grobs.eq.widths.aligned <- lapply(plots.grobs.eq.widths, function(x) {
      if (is.gtable(x$grobs[[8]])) {
        x$grobs[[8]] <- gtable_add_cols(x$grobs[[8]],
                                        unit(abs(diff(c(LegendWidth(x),
                                                        max.legends.width))),"mm"))
      }
      x
    })
    plots.grobs.eq.widths.aligned
  }
  if (is.null(xlabel)) xlabel=deparse(substitute(x))
  if (is.null(by)) {
    by=factor(rep("",length(x)))
    bylabel=""
  } else if (is.null(bylabel)) bylabel=deparse(substitute(by))
  dat=data.frame(x,by)
  if (faceted) dat$f = factor(rep(" ",nrow(dat)))
  stats <- aggregate(x~by, dat, function(x)
    c(mean=mean(x), sd=sd(x),bin=diff(range(x))/nclass.Sturges(x)))
  stats <- data.frame(by=stats[,1],stats[,2])
  bw=round(min(stats$bin[stats$bin>0]),1)
  gr<-ggplot(dat)
  if (boxplot)
    bp <- gr+ aes(x=by, y=x, fill=by, col=by) + geom_boxplot(alpha=.5)+
      xlab(bylabel) + ylab(xlabel) + ylim(range(x)) +
      stat_summary(fun.y=mean, geom="point", shape=22, size=3)
  if (histogram|densityCurve|normalCurve){
    gr <- gr+ aes(x=x, fill=by, col=by) #+ xlim(range(x))
    if (rug) gr<-gr+ geom_rug()
    if (histogram)
      gr<- gr + geom_histogram(aes(y=..density..),#,fill=by,col=by),
                               binwidth=bw,
                               alpha=.5, position="identity")
    if (densityCurve)
      gr<-gr + geom_density(aes(group=by),alpha=0.2,size=1)

    if (addMeanLine)
      gr<-gr+geom_vline(data=stats, aes(xintercept=mean,  colour=by),linetype="dashed", size=1)

    if (normalCurve){
      xmin=min(x)
      xmax=max(x)
      xx <- with(dat, seq(xmin, xmax, len=200))
      datn <- do.call(rbind,
                      lapply(1:nrow(stats),
                             function(i) with(stats[i,],
                                              data.frame(by, xx=c(xmin,xx,xmax),
                                                         y=c(0,dnorm(xx,mean=mean,sd=sd),0)))))
      gr<-gr+geom_line(data=datn, aes(xx, y,group=by,col=by),linetype=3,size=1)
      gr<-gr+geom_polygon(data=datn, aes(xx, y,group=by,fill=by),alpha=0.2,linetype=3,size=1)
      }

    if (faceted) gr <- gr + facet_grid(by~.) + guides(colour=FALSE, fill=FALSE)
    }
  if (boxplot&(histogram|densityCurve|normalCurve)){
    bp <- bp + coord_flip() + theme(plot.margin=unit(c(-0.2,0.2,0.2,0.2), "cm"))
    if (faceted) bp <- bp + guides(colour=FALSE, fill=FALSE) +facet_grid(f~.)
    gr <- gr + theme(axis.text.x=element_blank(),
                     axis.ticks.x=element_blank(),
                     plot.margin=unit(c(0.2,0.2,-0.2,0.2), "cm"))
    gp1 <- ggplotGrob(gr)
    gp2 <- ggplotGrob(bp)
    if (faceted){
      maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3])
      gp1$widths[2:3] <- maxWidth
      gp2$widths[2:3] <- maxWidth
      gr <- grid.arrange(gp1, gp2, ncol=1, nrow=2, widths=c(4), heights=c(3.5, 1.5))
    } else{
      plots=AlignPlots(gr,bp+guides(colour=FALSE, fill=FALSE) )
      gr<-grid.arrange(plots[[1]],plots[[2]],widths=c(4), heights=c(3.5, 1.5))
    }
  } else if (boxplot&!(histogram|densityCurve|normalCurve)) gr<- bp
  gr
  pandoc.p("")
}


