# @title Bivariate frequency table and barplot
# @description table2 builds a bivariate frequency table with counts and percentages
# at each combination of factor levels factor level and optionally shows a barplot.
# @param x variable to tabulate, which can be interpreted as factor
# @param by variable for crosstabulation, also interpreted as factor
# @param xlabel Label of the variable x, to be used in figures and tables.
# @param bylabel Label of the variable by, to be used in figures and tables.
# @param plot Logical. If TRUE a barplot is printed
# @param horizontal Logical. If TRUE bars are printed horizontally
# @param printFreq Logical. If TRUE frequency values are printed in the barplot
# @param density Logical. if TRUE barplot is printed with relative frequencies
# @param showTable Logical. If TRUE frequency table is printed via pander. If FALSE
# frequency table is returned invisibly.
# @param pctBycol Logical. If TRUE relative frequencies are computed by columns else by rows
# @return an univariate frequency table with absolute and relative frequencies, and
# optionally a barplot
# @examples
# symptom=sample(LETTERS[1:4],120,replace=TRUE)
# disease=sample(c("Yes","No"),120,replace=TRUE)
# table2(symptom,by=disease,horizontal=FALSE,density=TRUE,printFreq=TRUE,showTable=FALSE)
#
table2=function(x,by,xlabel=NULL,bylabel=NULL,plot=TRUE,horizontal=FALSE, printFreq=TRUE,
                density=TRUE, showTable=TRUE,pctBycol=TRUE, title=""){
  panderOptions('knitr.auto.asis', FALSE)
  panderOptions('keep.line.breaks', TRUE)
  panderOptions('table.style',"multiline")
  if (is.null(xlabel)) xlabel=toLabel(deparse(substitute(x)))
  if (is.null(bylabel)) bylabel=toLabel(deparse(substitute(by)))
  if (xlabel==bylabel) bylabel=paste(bylabel,"1",sep=".")
  x=factor(x)
  by=factor(by)
  tb=table(x,by)
  N=sum(tb)
  n=colSums(tb)
  along=if (pctBycol) 2 else 1
  pct=prop.table(tb,along)
  tbl2=data.frame(rownames(tb),array(sprintf("%4.0f (%.2f)", tb, 100*pct),dim=dim(tb)))
  #names(tbl2)=c(paste("\ ",bylabel,"\n",xlabel,"  \ "),paste(levels(by),"\n n (%)"))
  chip=tryCatch(chisq.test(tb),
                warning=function(e){
                  if (all(dim(tb)==c(2,2))) fisher.test(tb)
                  else chisq.test(tb,simulate.p.value = TRUE)
                })
  if (substr(chip$method,1,6)=="Fisher") test="Fisher exact test" else test="Chi-Squared test"
  pv=chip$p.value
  tbl1=table1(x[!is.na(by)],xlabel,plot=FALSE,showTable=FALSE)
  for (j in 2:ncol(tbl2)) levels(tbl2[,j])=c(levels(tbl2[,j]),"")
  tbl=cbind(tbl1,rbind(rep("",ncol(tbl2)-1),tbl2[,-1]))
  nmby=paste(bylabel," = ",levels(by),"\n(n=",n,")",sep="")
  ad=paste("All data\n(n=",N,")",sep="")
  wsp=rep("",nrow(tbl2))
  tbl=cbind(tbl,P=c(formatPval(pv),wsp))
  names(tbl)=c("Variable \n(levels)",ad,nmby,paste(test,"\nP"))
  if(showTable) pander(tbl, caption="Data are summarized in absolute frequencies and percentages, n(%)")
  if (plot){
    levels(x) <- gsub(" ", "\n", levels(x))
    freqTable=data.frame(tb)
    names(freqTable)=c("value","by","n")
    dpct=data.frame(pct)
    names(dpct)=c("value","by","pct")
    freqTable=merge(freqTable,dpct)
    levels(freqTable$value) <- gsub(" ", "\n", levels(freqTable$value))
    ldist=if (horizontal) 0.1 else 0.035
    if (density) {
      gr=ggplot(freqTable, aes(x=value, y=pct, fill=value)) +
        geom_bar(stat="identity") + facet_grid(by ~ .) +
        scale_y_continuous(labels=percent,limits=c(0,1.15*max(pct)))
    } else{
      gr=ggplot(freqTable, aes(x=value, y=n, fill=value)) +
        geom_bar(stat="identity") + facet_grid(by ~ .) +
        scale_y_continuous(limits=c(0,1.15*max(freqTable$n)))
    }
    gr= gr + xlab(xlabel) + ylab("Frequency") + ggtitle(title) +
      guides(fill=FALSE)
    if (horizontal) gr=gr+coord_flip()
    if (printFreq){
      if (length(levels(x))>5) tsz=3 else tsz=4
      if (density) gr=gr+geom_text(aes(label=paste0(round(pct*100,1),"%"),
                                       y=pct+ldist*max(pct)), size=tsz)
      else gr=gr+geom_text(aes(label=n, y=n+ldist*max(n)), size=tsz)
    }
    z <- ggplotGrob(gr)
    require(grid)
    require(gtable)
    # add label for right strip
    strips=which(z$layout$name=="strip-right")
    zwpos=strips[1]
    t=min(z$layout[strips,]$t)
    b=max(z$layout[strips,]$b)
    l=max(z$layout$l)+1
    z <- gtable_add_cols(z, z$widths[z$layout[zwpos, ]$l],length(z$widths) - 1)
    z <- gtable_add_grob(z,
                         list(rectGrob(gp = gpar(col = NA, fill = gray(0.8))),
                              textGrob(bylabel, rot = -90, gp = gpar(col = gray(0)))),
                         t, l, b, name = paste(runif(2)))
    plot(z)
    pandoc.p("")
  }
  return(invisible(tbl))
}
