# -----------------------------------------------------------------------------------
# Ejemplos de uso
# -----------------------------------------------------------------------------------
library(ULPGCmisc)
symptom=sample(LETTERS[1:4],120,replace=TRUE)
disease=sample(c("Yes","No"),120,replace=TRUE)

freqTable(symptom,by=disease,horizontal=FALSE,density=TRUE,printFreq=TRUE,showTable=TRUE)
freqTable(symptom,horizontal=FALSE,density=TRUE,printFreq=TRUE,showTable=FALSE,
          title="Frecuencia de aparición de los distintos síntomas")

x=rnorm(500,10,2)
summarize(x)
summarize(x,normalCurve=TRUE)
summarize(x,normalCurve=TRUE,report="complete")

x=rweibull(120,2,5);
summarize(x,densityCurve=TRUE,normalCurve=TRUE,rug=TRUE,report="complete")
summarize(x,densityCurve=TRUE,normalCurve=TRUE,report="complete",rug=FALSE)
summarize(x,densityCurve=TRUE,normalCurve=FALSE,rug=TRUE)
summarize(x,densityCurve=FALSE,normalCurve=FALSE,,report="complete",rug=FALSE,boxplot = FALSE)
summarize(x,boxplot = TRUE,histogram=FALSE)
summarize(x,report="complete")

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
summarize(x,by, report="complete",plot=TRUE)
summarize(x,by, plot=FALSE)


x=rnorm(100,10,2)
x=rweibull(100,10,2)
by=sample(c("Yes","No"),100,replace=TRUE,prob=c(0.7,0.3))
summarize(x,by=by)
summarize(x,by=by,normalCurve=TRUE,densityCurve=FALSE)


summarize(x,by=by)
summarize(x)
summarize(x,by=by, report="complete")
summarize(x, report="complete")

describe(x,report = "complete")
describe(x,by=by, report="complete")

symptom=sample(LETTERS[1:4],120,replace=TRUE)
disease=sample(c("Yes","No"),120,replace=TRUE)

describe(symptom,by=disease)
a=describe(symptom)


setwd("~/gDrive/BREIMAN/DoctoradoMedicina/Tareas R/")
library(openxlsx)
telde <- read.xlsx("endocrino.xlsx")
for (i in 1:ncol(telde)) if (length(unique(telde[,i]))==2) telde[,i]=factor(telde[,i])
telde$DM=ordered(telde$DM,levels=c(1,0),labels=c("DM+","DM-"))
telde$SEXO=ordered(telde$SEXO,levels=c(1,0),labels=c("Mujer","Hombre"))

freqTable(telde$SEXO)
freqTable(telde$DM,by=telde$SEXO)
with(telde,describe(A1C, by=DM, plot=TRUE))
with(telde,describe(A1C, by=DM, report="meansd",plot=TRUE))
describe(telde$TALLA, by=telde$HTA_OMS,plot=TRUE)

describe(telde[,1:10])

describe(telde[,1:10],by=telde$DM, pctBycol = FALSE)

describe(telde[,10:21], by=telde$DM)

dt <- describe(telde[,10:21], by=telde$DM)
pander(dt$nValid)

describe(telde)
dt<-describe(telde, by=telde$DM)
pander(dt$nValid)
