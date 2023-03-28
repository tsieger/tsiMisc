scaleToUnit<-structure(
function # Scale to unit range.
##description<<
## \code{\link{scaleToUnit}} scales the columns of a numeric matrix to
## the unit range.
(x, ##<< a numeric matrix
min = 0, ##<< the minimum in each dimension to scale to (defaults to
## \code{0})
max = 1, ##<< the maximum in each dimension to scale to (defaults to
## \code{1})
center = TRUE, ##<< shift and scale values to fit in the desired interval,
## or only scale them (and keep the mean fixed)?
solveSingular = TRUE ##<< if \code{TRUE}, constant columns will be
## transformed to the mean of \code{min} and \code{max}, not to
## \code{NaN}, as would result from a straighforward implementation
) {
  if (is.na(min) || is.na(max) || min>=max) {
    stop('invalid \'min\'/\'max\'')
  }

  if (!is.matrix(x)) x<-as.matrix(x)

  minX<-apply(x,2,min,na.rm=T)
  maxX<-apply(x,2,max,na.rm=T)
  rangeX<-maxX-minX
  if (solveSingular) {
    minX[rangeX==0]<-0
    maxX[rangeX==0]<-maxX[rangeX==0]*2
  }
  txImpl<-function(x,minX,maxX,min,max) {
    if (!is.matrix(x)) x<-as.matrix(x)
    if (nrow(x)>0) {
      if (center) {
        y<-apply(x,1,function(x) pmin(max,pmax(min,(max-min)*(x-minX)/(maxX-minX)+min)))
      } else {
        y<-apply(x,1,function(x) pmin(max,pmax(min,x/max(maxX/max,minX/min))))
      }
      if (ncol(x)>1) {
        y<-t(y)
      }
      if (!is.null(dim(y)) && dim(x)==dim(y)) dimnames(y)<-dimnames(x)
    } else {
      y<-x
    }
    return(y)
  }
  txInvImpl<-function(x,minX,maxX,min,max) {
    if (!is.matrix(x)) x<-as.matrix(x)
    if (nrow(x)>0) {
      if (center) {
        y<-apply(x,1,function(x) pmin(maxX,pmax(minX,(maxX-minX)*(x-min)/(max-min)+minX)))
      } else {
        y<-apply(x,1,function(x) pmin(maxX,pmax(minX,x*max(maxX/max,minX/min))))
      }
      if (ncol(x)>1) {
        y<-t(y)
      }
      dimnames(y)<-dimnames(x)
    } else {
      y<-x
    }
    return(y)
  }
  tx<-function(x) txImpl(x,minX,maxX,min,max)
  txInv<-function(x) txInvImpl(x,minX,maxX,min,max)

  x<-tx(x)

  attr(x,'min_x')<-minX
  attr(x,'max_x')<-maxX
  attr(x,'min')<-min
  attr(x,'max')<-max
  attr(x,'tx')<-tx
  attr(x,'txInv')<-txInv

  return(x)
  ### Scaled \code{x}. The desired minimum and maximum values
  ### are returned as attributes \code{min} and \code{max},
  ### respectively. The minimum/maximum values of \code{x} are returned
  ### as attributes \code{min_x} and \code{max_x}, respectively. The
  ### function used to transform a row in \code{x} to the desired range
  ### is returned as the \code{tx} attribute. The inverse transform can
  ### be find in the \code{txInv} attribute.
},ex=function() {

  # scale the \code{iris} data set
  x <- iris[, 1:4]
  summary(x)
  x2 <- scaleToUnit(x)
  summary(x2)

  # transform explicitly:
  x2[1, ]
  attr(x2, 'tx')(iris[1, 1:4])

  # inverse transform:
  y <- cbind(1:4,1)
  y
  y2 <- scaleToUnit(y ,min = 0, max = 1)
  y2
  y3 <- attr(y2, 'txInv')(y2)
  y3

  # scale, but not center
  scaleToUnit(1:10, min = -1, max = 1, center = FALSE)
  scaleToUnit(-2:5, min = -1, max = 1, center = FALSE)


})
