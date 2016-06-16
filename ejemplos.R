# -----------------------------------------------------------------------------------
# Ejemplos de uso
# -----------------------------------------------------------------------------------
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
