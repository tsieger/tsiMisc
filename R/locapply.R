locapply<-structure(
function # 
##description<<
(x, ##<< vector of x-coordinates
y, ##<< vector of y-coordinates
fun, ##<<
bandwidth=.2, ##<< bandwidth defining the neighbourhood in which to evaluate the quantiles
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
  return(z)
  ### A vector of .
},ex=function() {
  set.seed(1)
  n<-100
  x<-sort(runif(n,0,5))
  y<-runif(n,-exp(x),exp(x))
  plot(x,y,pch=19)
  lines(x,locapply(x,y,quantile,probs=.5))
  lines(x,locapply(x,y,quantile,probs=.9))
  lines(x,locapply(x,y,quantile,probs=.1))
  lines(x,locapply(x,y,quantile,probs=1))
  lines(x,locapply(x,y,quantile,probs=0))
})
