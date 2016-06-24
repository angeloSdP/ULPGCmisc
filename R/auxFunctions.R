# -------------------------------------
# Pretty formatting of p-values
# -------------------------------------
formatPval=function(pval){
  if (is.na(pval)) NA
  else if (pval<0.0001) "<0.0001"
  else as.character(sprintf("%5.4f",pval))
}
# -------------------------------------
# Returns Shapiro Test p-value or NA
# -------------------------------------
shap_pval=function(x){
  if (length(x)>5000) x=sample(x,5000)
  tryCatch(shapiro.test(x)$p.value, error=function(e) NA)
}
# -------------------------------------
# Returns mean and sd
# -------------------------------------
meansd=function(x) sprintf("%1.2f (%1.2f)", mean(x,na.rm=TRUE), sd(x,na.rm=TRUE))
# -------------------------------------
# Returns median and ptiles
# -------------------------------------
medianPtiles=function(x, ptiles) {
  sprintf("%1.1f (%1.1f;%1.1f)", median(x,na.rm=TRUE),
          quantile(x,na.rm=TRUE,probs=ptiles[1]),
          quantile(x,na.rm=TRUE,probs=ptiles[2]))
}
# -------------------------------------
# Full univariate report
# -------------------------------------
fullReport=function(x,xlabel=NULL){
  if (is.null(xlabel)) xlabel=deparse(substitute(x))
  q=quantile(x,prob=c(0.25,0.5,0.75),na.rm=TRUE)
  resumen=data.frame(
    Variable=xlabel,
    n.valid=length(na.omit(x)),
    missing=sum(is.na(x)),
    median=q[2],
    mean=mean(x,na.rm=TRUE),
    sd=sd(x,na.rm=TRUE),
    skewness=skewness(x,na.rm=TRUE),
    kurtosis=kurtosis(x,na.rm=TRUE),
    quartile1=q[1],
    quartile3=q[3],
    min=min(x,na.rm=TRUE),
    max=max(x,na.rm=TRUE)
  )
  rownames(resumen)=NULL
  resumen
}
# ---------------------------------------
# Generates a label from a variable name
# ---------------------------------------
toLabel=function(varname){
  if (grepl("[$]",varname)){
    svn= strsplit(varname,"[$]")[[1]]
    varname=svn[length(svn)]
  }
  varname
}
