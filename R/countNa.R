countNa<-structure(
function # Count NA's in a data frame.
##description<<
## 'countNa' computes the number of missing values in a data frame.
## It counts the number of missings in each column, the number of rows
## in which a value in at least one columns is missing, and the
## expected number of rows with at least one missing value (computed
## under the assumption of independence of missingness in individual
## columns). Number of rows left are also given.
## Optionally, combinations of columns reaching highest joint
## missingness is also reported (if 'combColCount > 1' and the
## number of columns in 'x' is at least 2).
(d, ##<< a data frame
sort = TRUE, ##<< sort columns of 'x' by the number of missings?
decreasing = FALSE, ##<< if sorting by the number of missing, should
## the sort be decreasing or increasing?
combColCount = 3 ##<< maximum number of columns to combine when finding a
## combination of columns reaching the highest number of missings
) {
  if (!is.data.frame(d)) d<-as.data.frame(d)
  n<-nrow(d)
  p<-ncol(d)
  missing<-vector('numeric',p+2)
  # number of NA's in each column
  missing[1:p]<-apply(d,2,function(x)sum(is.na(x)))
  # number of rows with at least one NA in them
  missing[p+1]<-n-nrow(na.omit(d))
  # average number of NA's
  missing[p+2]<-n*(1-prod(1-missing[1:p]/n)^(1/p))

  res<-data.frame(names=c(colnames(d),'any','expected'),
    missing=missing,
    missingPercent=missing/n*100,
    left=n-missing,
    leftPercent=(n-missing)/n*100)
  o<-order(res$missing[1:p],decreasing=decreasing)
  res<-res[c(o,p+1,p+2),]

  k<-min(combColCount,p)
  if (k>1) {
    o<-order(res$missing[1:p],decreasing=TRUE)
    bc<-e1071::bincombinations(k)
    # get rid of combinations of 0 or 1 column(s)
    bc<-bc[rowSums(bc)>1,]
   
    colComb<-data.frame(missing=numeric(nrow(bc)),n.col=numeric(nrow(bc)),str=character(nrow(bc)),stringsAsFactors=FALSE)
    max.present<-n
    max.idx<-NA
    for (i in 1:nrow(bc)) {
      flag.missing<-rep(F,n)
      s<-''
      for (i2 in 1:k) {
        if (bc[i,i2]) {
          flag.missing<-flag.missing|is.na(d[,o[k-i2+1]])
          s0<-colnames(d)[o[k-i2+1]]
          if (s!='') {
            s<-paste(s,s0,sep='+')
          } else {
            s<-s0
          }
        }
      }
      colComb$missing[i]<-sum(flag.missing)
      colComb$n.col[i]<-sum(bc[i,])
      colComb$str[i]<-s
    }
    # remove combinations of columns having no missings
    colComb<-colComb[colComb$missing>0,]
    # sort by increasing number of missings
    o2<-order(colComb$missing,-colComb$n.col,decreasing=T)
    colComb<-colComb[o2,]
    res<-list(columns=res,columnCombinations=data.frame(missing=colComb$missing,cols=colComb$str))
  }
  return(res)
  ### A data frame (or a list of two data frames) describing the
  ### missingness. The first data frame consists of rows describes the
  ### missingness in individual columns, plus the missingness in the
  ### combination of all columns (in a row called 'any'), plus the
  ### average missingness (in a row called 'average').
  ### The second data frame (if requested) describes the combinations
  ### of at most 'combColCount' columns of 'x' reaching highest joint
  ### missingness.
},ex=function() {
  d<-data.frame(x1=1,x2=2,x3=1:4,y=c(1,NA,2,NA),z=c(NaN,NaN,3,4))
  d
  countNa(d)
})
