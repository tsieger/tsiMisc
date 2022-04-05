lecdf<-structure(
function # Detrend data using local e.c.d.f.
##description<<
## Transform data by removing the trend from them and convert them to
## local e.c.d.f. (empirical cumulative distribution function).
## Each element of \code{y} gets transformed into a number in the unit interval,
## values near \code{0} represent relatively low values of \code{y} (in
## the context of their neighbourhood), values near \code{1} represent
## relatively high values of \code{y}.
(x, ##<< vector of x-coordinates
y, ##<< vector of y-coordinates
bandwidth=.2 ##<< bandwidth defining the neighbourhood in which to evaluate the quantiles
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
    z[ii]<-(sum(tmp[1]>=tmp)-1)/(length(tmp)-1)
  }
  return(z)
  ### A vector of local quantiles.
},ex=function() {
  set.seed(1)
  n<-100
  x<-runif(n,0,10)
  y<-sin(x)+runif(n)
  layout(rbind(1,2))
  # color points according to their vertical position
  # considering their local neighbourhood
  l<-lecdf(x,y,bandwidth=.1)
  plot(x,y,pch=19,col=topo.colors(100)[rank(l)],main='bandwidth=.1')
  # use larger bandwidth
  l<-lecdf(x,y,bandwidth=.5)
  plot(x,y,pch=19,col=topo.colors(100)[rank(l)],main='bandwidth=.5')
  layout(1)
})
