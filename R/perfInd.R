perfInd<-structure(
function # Compute performance indicators.
##description<<
## Compute several indicators to describe the performance of a binary
## classifier, e.g. sensitivity, specificity, an estimate of the area
## under the receiver operating characteristic, the Gini coefficient
## etc. (see the return value).
##
##seealso<< \code{link[ROCR]{performance}} which gives many more measures
##
##references<<
## Fawcett, Tom (2006). _An Introduction to ROC Analysis_. Pattern
## Recognition Letters 27 (8): 861874.
## doi:10.1016/j.patrec.2005.10.010.
## Powers, David M W (2011). _Evaluation: From Precision, Recall and
## F-Measure to ROC, Informedness, Markedness & Correlation._
## Journal of Machine Learning Technologies ISSN: 2229-3981 &
## ISSN: 2229-399X, Volume 2, Issue 1, 2011, pp-37-63
## Available online at \url{http://www.bioinfo.in/contents.php?id=51}
##
(x, ##<< a 2x2 classification table having predictions in rows, and
## ground truth in columns (negative cases come first, by default, see
## the \code{negativeFirst} argument), or a vector of predicted values
## for each observation (coded, by default, such that negative cases
## come first, e.g. as \code{0} and \code{1}, or \code{FALSE} and
## \code{TRUE}, or as a factor).
y = NULL, ##<< if \code{x} is a vector of predictions, \code{y} holds
## the vector of ground truth classification for each observation
## (coded, by default, such that negative cases come first, e.g. as
## \code{0} and \code{1}, or \code{FALSE} and \code{TRUE}, or as a
## factor).
negativeFirst = TRUE, ##<< if TRUE, negative cases come first in both
## rows and columns of \code{x}, or in vectors \code{x} and \code{y}
debug = FALSE ##<< if TRUE, debugs will be printed. If numeric of value
## greater than 1, verbose debugs will be produced.
) {
  if (!is.table(x)) {
    if (is.null(y)) {
      stop('\'x\' argmument not a table, expected \'y\'')
    }

    # determine names of 'x' and 'y'
    xname<-deparse(substitute(x))
    if (xname!=make.names(xname)) {
      xname<-'x'
    }
    yname<-deparse(substitute(y))
    if (yname!=make.names(yname)) {
      yname<-'y'
    }
    if (debug) .pn(xname)
    if (debug) .pn(yname)

    # if any of 'x' and 'y' has no two distinct values, fix it
    if (length(table(x))!=2 || length(table(y))!=2) {
      if (length(table(x))>2) {
        stop('\'x\' contains more than two distinct values')
      }
      if (length(table(y))>2) {
        stop('\'y\' contains more than two distinct values')
      }
      if (length(table(x))==1 && length(table(y))==1) {
        x<-as.factor(x)
        # create a surrogate level
        levels(x)<-c(levels(x),paste0(levels(x),'2'))
        y<-as.factor(y)
        levels(y)<-c(levels(y),paste0(levels(y),'2'))
      } else if (length(table(x))==1) {
        y<-as.factor(y)
        if (levels(as.factor(x))%in%levels(y)) {
          # take the levels of 'y' for 'x'
          x<-factor(x,levels=levels(y))
        } else {
          x<-as.factor(x)
          # create a surrogate level
          levels(x)<-c(levels(x),paste0(levels(x),'2'))
        }
      } else {
        x<-as.factor(x)
        if (levels(as.factor(y))%in%levels(x)) {
          # take the levels of 'x' for 'y'
          y<-factor(y,levels=levels(x))
        } else {
          x<-as.factor(x)
          # create a surrogate level
          levels(x)<-c(levels(x),paste0(levels(x),'2'))
        }
      }
    }

    txt<-paste0('table(',xname,'=x,',yname,'=y)')
    if (debug) .pn(txt)
    x<-eval(parse(text=txt))
  } else {
    if (length(dim(x))!=2 || !all(dim(x)==c(2,2))) {
      stop('2x2 table expected')
    }
  }
  if (!negativeFirst) {
    x<-flip(x,1:2)
  }

  # convert to double to avoid integer ovewflow
  storage.mode(x)<-'double'

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

  ##value<< a list containing the following named entries:
  res<-list(
    table=x, ##<< classification table
    sensitivity=sensitivity, ##<< sensitivity
    specificity=specificity, ##<< specificity
    npv=npv, ##<< negative predicted value
    ppv=ppv, ##<< positive predicted value
    wnpv=wnpv, ##<< weighted negative predicted value (an obscure measure)
    wppv=wppv, ##<< weighted positive predicted value
    fpr=fpr, ##<< false positive rate
    fnr=fnr, ##<< false negative rate
    fdr=fdr, ##<< false discovery rate
    accuracy=acc, ##<< accuracy
    f1=f1, ##<< F1 score
    auc=auc, ##<< AUC (area under the ROC curve estimated by
    ## interpolating the (0,0), (1-specificity, sensitivity) and 
    ## (1, 1) points in the ROC space)
    gini=gini, ##<< the Gini index
    n=n) ##<< number of observations classified
  return(res)
  ##<< end
},ex=function() {
  # example from https://en.wikipedia.org/w/index.php?title=Sensitivity_and_specificity&oldid=680316530
  x<-matrix(c(20,10,180,1820),2)
  print(x)
  perfInd(x)

  # compute perormance over a vector of predicted and true classification:
  print(perfInd(c(0,0,1,1,1), c(0,0,0,1,1)))
})
