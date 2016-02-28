scaleToUnit<-structure(
function # Scale the columns of a numeric matrix to the unit range.
(x, ##<< numeric matrix
min = 0, ##<< minimum in each dimension to scale to  (defaults to 0)
max = 1, ##<< maximum in each dimension to scale to (defaults to 1)
singular = TRUE ##<< if TRUE, constant columns will be transformed to
## the mean of 'min', 'max', not to NaN, as would result from a
## straighforward implementation
) {
  if (is.na(min) || is.na(max) || min>=max) {
    stop('invalid \'min\'/\'max\'')
  }

  if (!is.matrix(x)) x<-as.matrix(x)

  minX<-apply(x,2,min,na.rm=T)
  maxX<-apply(x,2,max,na.rm=T)
  rangeX<-maxX-minX
  if (singular) {
    minX[rangeX==0]<-0
    maxX[rangeX==0]<-maxX[rangeX==0]*2
  }
  txImpl<-function(x,minX,maxX,min,max) {
    if (!is.matrix(x)) x<-as.matrix(x)
    y<-apply(x,1,function(x) pmin(max,pmax(min,(max-min)*(x-minX)/(maxX-minX)+min)))
    if (ncol(x)>1) {
      y<-t(y)
    }
    dimnames(y)<-dimnames(x)
    return(y)
  }
  tx<-function(x) txImpl(x,minX,maxX,min,max)

  x<-tx(x)

  attr(x,'scaleToUnit:min_x')<-minX
  attr(x,'scaleToUnit:max_x')<-maxX
  attr(x,'scaleToUnit:min')<-min
  attr(x,'scaleToUnit:max')<-max
  attr(x,'scaleToUnit:tx')<-tx
  attr(x,'scaleToUnit:txImpl')<-txImpl

  return(x)
  ### Scaled version of 'x'. The desired minimum and maximum values
  ### are returned as attributes 'min' and 'max', respectively. The
  ### minimum/maximum values of 'x' are returned as attributes 'min_x'
  ### and 'max_x', respectively. The function used to transform a row
  ### in 'x' to the desired range is returned as 'tx' attribute.
},ex=function() {
  (x <- scaleToUnit(iris[, 1:4]))
  summary(x)

  x[1, ]
  attr(x, 'scaleToUnit:tx')(iris[1, 1:4])

  scaleToUnit(cbind(1:4,1),min=0,max=1)
})
