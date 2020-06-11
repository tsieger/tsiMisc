perfInd<-structure(
function # Compute performance indicators.
##description<<
## Compute several indicators to describe the performance of a binary
## classifier, e.g. sensitivity, specificity, an estimate of the area
## under the receiver operating characteristic, the Gini coefficient
## etc. (see the return value).
##
##seealso<< \code{\link[ROCR]{performance}} which gives many more measures
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
## \code{TRUE}, or as a factor). If a row (and a column) is missing,
## a guess is made to make a 2x2 table from \code{x}. This requires
## \code{x} to have rows/columns named as 'TRUE' and 'FALSE' such that
## we would deduce which row/column is missing. See examples.
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
  if (is.matrix(x)) {
    x<-as.table(x)
  }
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
          y<-as.factor(y)
          # create a surrogate level
          levels(y)<-c(levels(y),paste0(levels(y),'2'))
        }
      }
    }

    txt<-paste0('table(',xname,'=x,',yname,'=y)')
    if (debug) .pn(txt)
    x<-eval(parse(text=txt))
  } else {
    if (length(dim(x))!=2 || !all(dim(x)==c(2,2))) {
      if (length(dim(x))==2) {
        # hack: fix the special case of degenerated classification with two rows
        # but only one column with known missing column
        if (dim(x)[1]==1 && rownames(x)%in%c('FALSE','TRUE')) {
          dn<-dimnames(x)
          dn[[1]]<-c('FALSE','TRUE')
          tmp<-matrix(0,1,ncol(x))
          if (rownames(x)[1]=='FALSE') {
            x<-as.table(rbind(x,tmp))
          } else if (rownames(x)[1]=='TRUE') {
            x<-as.table(rbind(tmp,x))
          }
          dimnames(x)<-dn
        }
        # hack: fix the special case of degenerated classification with two columns
        # but only one row with known missing row
        if (dim(x)[2]==1 && colnames(x)%in%c('FALSE','TRUE')) {
          dn<-dimnames(x)
          dn[[2]]<-c('FALSE','TRUE')
          tmp<-matrix(0,nrow(x),1)
          if (colnames(x)[1]=='FALSE') {
            x<-as.table(cbind(x,tmp))
          } else if (colnames(x)[1]=='TRUE') {
            x<-as.table(cbind(tmp,x))
          }
          dimnames(x)<-dn
        }
      }
    }
    if (length(dim(x))!=2 || !all(dim(x)==c(2,2))) {
      stop('2x2 table expected')
    }
  }
  if (negativeFirst) {
    x<-flip(x,1:2)
  }
  tbl<-flip(x,1:2)

  # convert to double to avoid integer overflow
  storage.mode(x)<-'double'

  tp<-tbl[2,2]
  tn<-tbl[1,1]
  fn<-tbl[1,2]
  fp<-tbl[2,1]
  # convert to doubles to overcome integer overflow
  tp<-as.double(tp)
  tn<-as.double(tn)
  fn<-as.double(fn)
  fp<-as.double(fp)
  sensitivity<-tp/(tp+fn)
  specificity<-tn/(tn+fp)
  ppv<-        tp/(tp+fp)
  npv<-        tn/(tn+fn)
  wppv<-ppv*(tp+fn)/sum(x)
  wnpv<-npv*(tn+fp)/sum(x)
  fpr<-1-specificity
  fnr<-1-sensitivity
  fdr<-1-ppv
  acc<-sum(diag(x))/sum(x)
  f1<-2*tp/(2*tp+fn+fp)
  f2<-5*tp/(5*tp+4*fn+fp)
  f05<-1.25*tp/(1.25*tp+0.25*fn+fp)
  correspondence<-tp/(tp+fn+fp)
  mcc<-(tp*tn-fp*fn)/sqrt((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn))
  informedness<-sensitivity+specificity-1
  markedness<-ppv+npv-1
  auc<-sensitivity*(1-specificity)/2+specificity*sensitivity+specificity*(1-sensitivity)/2
  gini<-2*auc-1
  n=sum(x)

  ##value<< a list containing the following named entries:
  res<-list(
    table=tbl, ##<< classification table
    tp=tbl[2,2], ##<< true positives
    tn=tbl[1,1], ##<< true negatives
    fn=tbl[1,2], ##<< false negatives
    fp=tbl[2,1], ##<< false positives
    sensitivity=sensitivity, ##<< sensitivity
    specificity=specificity, ##<< specificity
    npv=npv, ##<< negative predicted value, i.e. true negatives over true negatives and false positives
    ppv=ppv, ##<< positive predicted value, i.e. true positives over true positives and false negatives
    wnpv=wnpv, ##<< weighted negative predicted value (an obscure measure)
    wppv=wppv, ##<< weighted positive predicted value
    fpr=fpr, ##<< false positive rate
    fnr=fnr, ##<< false negative rate
    fdr=fdr, ##<< false discovery rate
    accuracy=acc, ##<< accuracy
    f1=f1, ##<< F1 score (2*TP/(2*TP+FN+FP))
    f2=f2, ##<< F2 score (5*TP/(5*TP+4*FN+FP))
    f05=f05, ##<< F0.5 score (1.25*TP/(1.25*TP+0.25*FN+FP))
    correspondence=correspondence, ##<< correspondence score (TP/(TP+FN+FP))
    mcc=mcc, ##<< Matthews correlation coefficient
    informedness=informedness, ##<< informedness
    markedness=markedness, ##<< markedness
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

  # compare several measures over several classification results:
  tbls<-list(matrix(c(98,2,2,8),2),matrix(c(8,2,2,8),2),matrix(c(80,20,2,8),2))
  for (i in 1:length(tbls)) {
    m<-tbls[[i]]
    .pn(m)
    pi<-perfInd(m)
    with(pi,catnl(
      'acc: ',accuracy,
      ', sp+se: ',sensitivity+specificity,
      ', correspondence: ',correspondence,
      ', F1: ',f1,
      ', F2: ',f2,sep=''))
  }

  # make degenerated table proper 2x2 table
  i1<-c(1,1,1,1)
  i2<-c(1,1,2,2)
  m<-table(i1==1,i2==1)
  print(m) # this is 1x2 table
  perfInd(m)$table # this is 2x2 table
})
