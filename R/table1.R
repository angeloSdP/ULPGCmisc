# @title Univariate frequency table and barplot
# @description table1 builds a univariate frequency table with counts and percentages
# at each factor level, and optionally shows a barplot.
# @param x Variable to tabulate, which can be interpreted as factor
# @param xlabel Label of the variable x, to be used in figures and tables.
# @param plot Logical. If TRUE a barplot is printed
# @param horizontal Logical. If TRUE bars are printed horizontally
# @param printFreq Logical. If TRUE frequency values are printed in the barplot
# @param density Logical. if TRUE barplot is printed with relative frequencies
# @param showTable Logical. If TRUE frequency table is printed via pander. If FALSE
# frequency table is returned invisibly.
# @return an univariate frequency table with absolute and relative frequencies, and
# optionally a barplot
# @examples
# letters=sample(LETTERS[1:4],120,replace=TRUE)
# table1(letters,xlabel="letters in a text",horizontal=TRUE,density=TRUE,printFreq=FALSE, showTable=TRUE)
#
table1=function(x,xlabel=NULL,plot=TRUE,horizontal=FALSE, printFreq=TRUE,
density=TRUE, showTable=TRUE){
  panderOptions('knitr.auto.asis', FALSE)
  panderOptions('keep.line.breaks', TRUE)
  panderOptions('table.style',"multiline")
  if (is.null(xlabel)) xlabel=toLabel(deparse(substitute(x)))
  if (!is.factor(x)) x=factor(x)
  n=table(x)
  pct=prop.table(n)
  tbl=data.frame(rownames(n),sprintf("%4.0f (%.2f)", n, 100*pct))
  names(tbl)=c(xlabel,"n(%)")
  if (showTable) pander(tbl)
  if (plot){
    freqTable=data.frame(value=rownames(n),cbind(n,pct))
    levels(freqTable$value) <- gsub(" ", "\n", levels(freqTable$value))
    ldist=if (horizontal) 0.045 else 0.035
    if (density) {
      gr=ggplot(freqTable, aes(x=value, y=pct, fill=value)) +
        geom_bar(stat="identity") +
        scale_y_continuous(labels=percent,limits=c(0,1.1*max(pct)))+
        scale_x_discrete(limits=levels(x))
    } else{
      gr=ggplot(freqTable, aes(x=value, y=n, fill=value)) +
        geom_bar(stat="identity") +
        scale_y_continuous(limits=c(0,1.1*max(n)))+
        scale_x_discrete(limits=levels(x))
    }
    gr= gr + xlab(xlabel) + ylab("Frequency") + ggtitle(xlabel) +
      guides(fill=FALSE)
    if (horizontal) gr=gr+coord_flip()
    if (printFreq){
      if (density) gr=gr+geom_text(aes(label=paste0(round(pct*100,1),"%"),
                                       y=pct+ldist*max(pct)), size=4)
      else gr=gr+geom_text(aes(label=n, y=n+ldist*max(n)), size=4)
    }
    print(gr)
  }
  # formatting table for invisible returning
  tbl[[1]]=as.character(tbl[[1]])
  levels(tbl[[2]])=c(levels(tbl[[2]]),"")
  tbl=rbind(c(xlabel,""),tbl)
  xwidth=max(nchar(tbl[[1]]))+5
  tbl[1,1]=str_pad(tbl[1,1],xwidth,"right")
  tbl[-1,1]=str_pad(tbl[-1,1],xwidth,"left")
  names(tbl)[1]=""
  return(invisible(tbl))
}
