perfInd<-structure(
function # Compute performance indicators.
##description<<
## Compute several indicators to describe the performance of a binary
## classifier, e.g. sensitivity, specificity, an estimate of the area
## under the receiver operating characteristic, the Gini coefficient
## etc. (see the return value).
##
##seealso<< 'ROCR::performance' which gives many more measures
##
##references<<
## Fawcett, Tom (2006). _An Introduction to ROC Analysis_. Pattern
## Recognition Letters 27 (8): 861874.
## doi:10.1016/j.patrec.2005.10.010.
## Powers, David M W (2011). _Evaluation: From Precision, Recall and
## F-Measure to ROC, Informedness, Markedness & Correlation._
## Journal of Machine Learning Technologies ISSN: 2229-3981 &
## ISSN: 2229-399X, Volume 2, Issue 1, 2011, pp-37-63
## Available online at http://www.bioinfo.in/contents.php?id=51
(x, ##<< a 2x2 classification table having predictions in rows, and
## ground truth in columns
negativeFirst = FALSE ##<< if TRUE, positive case come first in both
## rows and columns
) {
  if (negativeFirst) {
    x<-flip(x,1:2)
  }
  sensitivity<-x[1,1]/sum(x[,1])
  specificity<-x[2,2]/sum(x[,2])
  ppv<-        x[1,1]/sum(x[1,])
  npv<-        x[2,2]/sum(x[2,])
  wppv<-ppv*sum(x[,1])/sum(x)
  wnpv<-npv*sum(x[,2])/sum(x)
  fpr<-1-specificity
  fnr<-1-sensitivity
  fdr<-1-ppv
  acc<-sum(diag(x))/sum(x)
  f1<-2*sensitivity*ppv/(sensitivity+ppv)
  mcc<-(x[1,1]*x[2,2]-x[1,2]*x[2,1])/sqrt(as.double(sum(x[1,]))*sum(x[2,])*sum(x[,1])*sum(x[,2]))
  informedness<-sensitivity+specificity-1
  markedness<-ppv+npv-1
  auc<-sensitivity*(1-specificity)/2+specificity*sensitivity+specificity*(1-sensitivity)/2
  gini<-2*auc-1
  n=sum(x)
  res<-data.frame(sensitivity=sensitivity,
    specificity=specificity,
    npv=npv,
    ppv=ppv,
    wnpv=wnpv,
    wppv=wppv,
    fpr=fpr,
    fnr=fnr,
    fdr=fdr,
    acc=acc,
    f1=f1,
    auc=auc,
    gini=gini,
    n=n)
  return(res)
  ### A row matrix containing named entries of 'sensitivity',
  ### 'specificity', 'npv' (negative predictive value), 'ppv',
  ### 'wnpv' (weighted npv, an obscure measure), 'wppv',
  ### 'fpr' (falso positive rate), 'fnr', 'fdr' (false discovery rate),
  ### 'acc' (accuracy), 'f1', 'auc' (area under the ROC curve estimated
  ### by interpolating the (0,0), (1-specificity, sensitivity) and 
  ### (1, 1) points in the ROC space), 'gini' (the Gini index), and 'n'
  ### (number of classified cases).
},ex=function() {
  # example from https://en.wikipedia.org/w/index.php?title=Sensitivity_and_specificity&oldid=680316530
  x<-matrix(c(20,10,180,1820),2)
  print(x)
  perfInd(x)
})
