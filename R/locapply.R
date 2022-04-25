locapply<-structure(
function # Apply function to data locally.
##description<<
## \code{\link{locapply}} applies given function to portions of \code{y}
## in the local \code{x} neighbourhood. This generalizes e.g. \code{lowess}
## as you can compute any function of \code{y}, not only the mean.
(x, ##<< vector of x-coordinates
y, ##<< vector of y-coordinates
fun, ##<< function to be applied; it will be passed a vector of y coordinates
## in the neighbourhood of some x
bandwidth=.2, ##<< bandwidth defining the neighbourhood in x-coordinates
... ##<< additional arguments to \code{fun}
) {
  if (length(x)!=length(y)) stop('length of x and y differ')
  if (any(is.na(x))) stop('x contains NAs, UNIMPLEMENTED, TODO')

  n<-length(x)

  z<-rep(NA,n)
  d<-diff(range(x))*bandwidth
  nonna<-which(!is.na(y))
  y2<-y[nonna]
  x2<-x[nonna]
  #TODO:optimize
  for (i in seq(along=nonna)) {
    ii<-nonna[i]
    # indices of samples in the neighbourhood of ii
    idx<-c(ii,setdiff(which(x2>x2[ii]-d & x2<x2[ii]+d),ii))
    tmp<-y2[idx]
    z[ii]<-fun(tmp,...)
  }
  return(list(x=x2,y=z))
  ### A list of vectors \code{x} and \code{y} holding the x-coordinates,
  ### and the result of \code{fun} applied to the local neighbourhood
  ### of corresponding element of \code{x}.
},ex=function() {
  set.seed(1)
  n<-100
  x<-sort(runif(n,0,5))
  y<-runif(n,-exp(x),exp(x))
  plot(x,y,pch=19)
  lines(x,locapply(x,y,quantile,probs=.5)$y)
  lines(x,locapply(x,y,quantile,probs=.9)$y)
  lines(x,locapply(x,y,quantile,probs=.1)$y)
  lines(x,locapply(x,y,quantile,probs=1)$y)
  lines(x,locapply(x,y,quantile,probs=0)$y)
})
