# -----------------------------------------------------------------------------------
# Ejemplos de uso
# -----------------------------------------------------------------------------------
library(ULPGCmisc)
symptom=sample(LETTERS[1:4],120,replace=TRUE)
disease=sample(c("Yes","No"),120,replace=TRUE)
x=symptom
by=disease
xname=NULL
byname=NULL

t1=table1(symptom,horizontal=TRUE,density=TRUE,printFreq=FALSE, showTable=TRUE)
t1
t2=table2(symptom,by=disease,horizontal=FALSE,density=TRUE,printFreq=TRUE,showTable=FALSE)
t2

freqTable(symptom,by=disease,horizontal=FALSE,density=TRUE,printFreq=TRUE,showTable=TRUE)
freqTable(symptom,horizontal=FALSE,density=TRUE,printFreq=TRUE,showTable=FALSE)

x=rnorm(50,10,2)
summary1(x)
summary1(x,normalCurve=TRUE,fullreport=TRUE)

x=rweibull(120,2,5);
summary1(x,densityCurve=TRUE,normalCurve=TRUE,rug=TRUE)
summary1(x,densityCurve=TRUE,normalCurve=TRUE,fullreport=TRUE,rug=FALSE)
summary1(x,densityCurve=TRUE,normalCurve=FALSE,rug=TRUE)
summary1(x,densityCurve=FALSE,normalCurve=FALSE,,fullreport=TRUE,rug=FALSE,boxplot = FALSE)

plotSummary2(x,by=NULL,boxplot=TRUE,histogram=TRUE)

# Gráficos summary2
x=rweibull(120,2,5)
by=sample(c("yes","no"),120,replace=TRUE,prob=c(0.6,0.4))

plotSummary2(x,by, boxplot=TRUE,histogram=FALSE)
plotSummary2(x,by, boxplot=FALSE,histogram=TRUE)
plotSummary2(x,by, boxplot=TRUE,histogram=TRUE)
plotSummary2(x,by, boxplot=TRUE,histogram=TRUE,faceted=TRUE)
plotSummary2(x,by, boxplot=TRUE,histogram=FALSE,densityCurve=TRUE)
plotSummary2(x,by, boxplot=TRUE,histogram=TRUE,normalCurve=TRUE)
plotSummary2(x,by, boxplot=TRUE,histogram=FALSE,normalCurve=TRUE, rug=FALSE)
plotSummary2(x,by, boxplot=TRUE,histogram=FALSE,densityCurve=TRUE,faceted=TRUE, rug=FALSE)
plotSummary2(x,by, boxplot=TRUE,histogram=TRUE,densityCurve=TRUE,faceted=TRUE)
plotSummary2(x,by, boxplot=TRUE,histogram=TRUE,normalCurve=TRUE,faceted=TRUE)
plotSummary2(x,by, boxplot=TRUE,histogram=FALSE,normalCurve=TRUE,faceted=TRUE, rug=FALSE)
plotSummary2(x,by, boxplot=TRUE,histogram=FALSE,normalCurve=TRUE,densityCurve=TRUE,faceted=TRUE)
plotSummary2(x,by, boxplot=TRUE,histogram=TRUE,normalCurve=TRUE,densityCurve=TRUE,faceted=TRUE)
plotSummary2(x,by, boxplot=TRUE,histogram=TRUE,normalCurve=TRUE,densityCurve=TRUE,faceted=FALSE)

x=c(rnorm(50,10,2),rnorm(50,8,1),rnorm(50,14,3),rnorm(50,10,0.5))
by=sort(rep(letters[1:4],50))
plotSummary2(x,by, boxplot=TRUE,histogram=FALSE)
plotSummary2(x,by, boxplot=FALSE,histogram=TRUE)
plotSummary2(x,by, boxplot=TRUE,histogram=TRUE)
plotSummary2(x,by, boxplot=TRUE,histogram=TRUE,faceted=TRUE)
plotSummary2(x,by, boxplot=TRUE,histogram=FALSE,densityCurve=TRUE)
plotSummary2(x,by, boxplot=TRUE,histogram=TRUE,normalCurve=TRUE)
plotSummary2(x,by, boxplot=TRUE,histogram=FALSE,normalCurve=TRUE, rug=FALSE)
plotSummary2(x,by, boxplot=TRUE,histogram=FALSE,densityCurve=TRUE,faceted=TRUE, rug=FALSE)
plotSummary2(x,by, boxplot=TRUE,histogram=TRUE,densityCurve=TRUE,faceted=TRUE)
plotSummary2(x,by, boxplot=TRUE,histogram=TRUE,normalCurve=TRUE,faceted=TRUE)
plotSummary2(x,by, boxplot=TRUE,histogram=FALSE,normalCurve=TRUE,faceted=TRUE, rug=FALSE)
plotSummary2(x,by, boxplot=TRUE,histogram=FALSE,normalCurve=TRUE,densityCurve=TRUE,faceted=TRUE)
plotSummary2(x,by, boxplot=TRUE,histogram=TRUE,normalCurve=TRUE,densityCurve=TRUE,faceted=TRUE)
plotSummary2(x,by, boxplot=TRUE,histogram=TRUE,normalCurve=TRUE,densityCurve=TRUE,faceted=FALSE)

x=c(rnorm(50,10,2),rnorm(50,8,1),rnorm(50,14,3),rnorm(50,10,0.5))
by=sort(rep(letters[1:4],50))
summary2(x,by, fullreport=TRUE,plot=FALSE)
summary2(x,by, fullreport=FALSE,plot=FALSE)


                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               x=rnorm(100,10,2)
x=rweibull(100,10,2)
by=sample(c("Yes","No"),100,replace=TRUE,prob=c(0.7,0.3))
summary2(x,by=by)
summary(x,by=by,normalCurve=TRUE,densityCurve=FALSE)


summarize(x,by=by)
summarize(x)
summarize(x,by=by, fullreport=TRUE)
summarize(x, fullreport=TRUE)

describe(x,fullreport = TRUE)
describe(x,by=by, fullreport=TRUE)

symptom=sample(LETTERS[1:4],120,replace=TRUE)
disease=sample(c("Yes","No"),120,replace=TRUE)

describe(symptom,by=disease)
describe(symptom)

