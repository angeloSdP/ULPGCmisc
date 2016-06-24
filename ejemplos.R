# -----------------------------------------------------------------------------------
# Ejemplos de uso
# -----------------------------------------------------------------------------------
library(ULPGCmisc)
symptom=sample(LETTERS[1:4],120,replace=TRUE)
disease=sample(c("Yes","No"),120,replace=TRUE)

freqTable(symptom,by=disease,horizontal=FALSE,density=TRUE,printFreq=TRUE,showTable=TRUE)
freqTable(symptom,horizontal=FALSE,density=TRUE,printFreq=TRUE,showTable=FALSE)

x=rnorm(500,10,2)
summarize(x)
summarize(x,normalCurve=TRUE)
summarize(x,normalCurve=TRUE,fullreport=TRUE)

x=rweibull(120,2,5);
summarize(x,densityCurve=TRUE,normalCurve=TRUE,rug=TRUE,fullreport=TRUE)
summarize(x,densityCurve=TRUE,normalCurve=TRUE,fullreport=TRUE,rug=FALSE)
summarize(x,densityCurve=TRUE,normalCurve=FALSE,rug=TRUE)
summarize(x,densityCurve=FALSE,normalCurve=FALSE,,fullreport=TRUE,rug=FALSE,boxplot = FALSE)
summarize(x,boxplot = TRUE,histogram=FALSE)
summarize(x,fullreport=TRUE)

# Gráficos summarize
x=rweibull(120,2,5)
by=sample(c("yes","no"),120,replace=TRUE,prob=c(0.6,0.4))

summarize(x,by, boxplot=TRUE,histogram=FALSE,faceted=FALSE)
summarize(x,by, boxplot=FALSE,histogram=TRUE,faceted=FALSE)
summarize(x,by, boxplot=TRUE,histogram=TRUE,faceted=FALSE)
summarize(x,by, boxplot=TRUE,histogram=TRUE,faceted=TRUE)
summarize(x,by, boxplot=TRUE,histogram=FALSE,densityCurve=TRUE)
summarize(x,by, boxplot=TRUE,histogram=TRUE,normalCurve=TRUE)
summarize(x,by, boxplot=TRUE,histogram=FALSE,normalCurve=TRUE, rug=FALSE)
summarize(x,by, boxplot=TRUE,histogram=FALSE,densityCurve=TRUE,faceted=TRUE, rug=FALSE)
summarize(x,by, boxplot=TRUE,histogram=TRUE,densityCurve=TRUE,faceted=TRUE)
summarize(x,by, boxplot=TRUE,histogram=TRUE,normalCurve=TRUE,faceted=TRUE)
summarize(x,by, boxplot=TRUE,histogram=FALSE,normalCurve=TRUE,faceted=TRUE, rug=FALSE)
summarize(x,by, boxplot=TRUE,histogram=FALSE,normalCurve=TRUE,densityCurve=TRUE,faceted=TRUE)
summarize(x,by, boxplot=TRUE,histogram=TRUE,normalCurve=TRUE,densityCurve=TRUE,faceted=TRUE)
summarize(x,by, boxplot=TRUE,histogram=TRUE,normalCurve=TRUE,densityCurve=TRUE,faceted=FALSE)

x=c(rnorm(50,10,2),rnorm(50,8,1),rnorm(50,14,3),rnorm(50,10,0.5))
by=sort(rep(letters[1:4],50))
summarize(x,by, boxplot=TRUE,histogram=FALSE,faceted=FALSE)
summarize(x,by, boxplot=FALSE,histogram=TRUE,faceted=FALSE)
summarize(x,by, boxplot=TRUE,histogram=TRUE,faceted=FALSE)
summarize(x,by, boxplot=TRUE,histogram=TRUE,faceted=TRUE)
summarize(x,by, boxplot=TRUE,histogram=FALSE,densityCurve=TRUE)
summarize(x,by, boxplot=TRUE,histogram=TRUE,normalCurve=TRUE)
summarize(x,by, boxplot=TRUE,histogram=FALSE,normalCurve=TRUE, rug=FALSE)
summarize(x,by, boxplot=TRUE,histogram=FALSE,densityCurve=TRUE,faceted=TRUE, rug=FALSE)
summarize(x,by, boxplot=TRUE,histogram=TRUE,densityCurve=TRUE,faceted=TRUE)
summarize(x,by, boxplot=TRUE,histogram=TRUE,normalCurve=TRUE,faceted=TRUE)
summarize(x,by, boxplot=TRUE,histogram=FALSE,normalCurve=TRUE,faceted=TRUE, rug=FALSE)
summarize(x,by, boxplot=TRUE,histogram=FALSE,normalCurve=TRUE,densityCurve=TRUE,faceted=TRUE)
summarize(x,by, boxplot=TRUE,histogram=TRUE,normalCurve=TRUE,densityCurve=TRUE,faceted=TRUE)
summarize(x,by, boxplot=TRUE,histogram=TRUE,normalCurve=TRUE,densityCurve=TRUE,faceted=FALSE)

x=c(rnorm(50,10,2),rnorm(50,8,1),rnorm(50,14,3),rnorm(50,10,0.5))
by=sort(rep(letters[1:4],50))
summarize(x,by, fullreport=TRUE,plot=TRUE)
summarize(x,by, fullreport=FALSE,plot=FALSE)


                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               x=rnorm(100,10,2)
x=rweibull(100,10,2)
by=sample(c("Yes","No"),100,replace=TRUE,prob=c(0.7,0.3))
summarize(x,by=by)
summarize(x,by=by,normalCurve=TRUE,densityCurve=FALSE)


summarize(x,by=by)
summarize(x)
summarize(x,by=by, fullreport=TRUE)
summarize(x, fullreport=TRUE)

describe(x,fullreport = TRUE)
describe(x,by=by, fullreport=TRUE)

symptom=sample(LETTERS[1:4],120,replace=TRUE)
disease=sample(c("Yes","No"),120,replace=TRUE)

describe(symptom,by=disease)
a=describe(symptom)

library(openxlsx)
setwd("~/gDrive/cursoRMedicina/")
telde <- read.xlsx("endocrino.xlsx",sheet=1)
library(ULPGCmisc)
with(telde,describe(A1C, by=DM))

