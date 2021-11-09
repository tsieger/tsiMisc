histdn<-structure(
function # Histogram with defaults and normal density approximation.
##description<<
## \code{\link{histd}} is just a shortcut to \code{\link[graphics]{hist}}
## passing it some default values, with a Gaussian curve of normal approximation
## superposed onto it.
(x, ##<< a vector of values for which the histogram is desired
breaks = 100, ##<< histogram breaks, see \code{\link[graphics]{hist}}
col = 'gray', ##<< a color to fill the bars, defaults to gray, see \code{\link[graphics]{hist}}
coln = 'red', ##<< a color of normal density estimate
main = NULL, ##<<
xlab = NULL, ##<<
... ##<< other parameters to \code{\link[graphics]{hist}}
) {
  if (is.null(main)) main<-paste('Histogram of',deparse(substitute(x)))
  if (is.null(xlab)) xlab<-deparse(substitute(x))
  h<-hist(x,breaks=breaks,col=col,main=main,xlab=xlab,...)
  y<-dnorm(h$mids,mean(x,na.rm=T),sd(x,na.rm=T))*diff(h$mids[1:2])*sum(!is.na(x))
  lines(h$mids,y,col=coln)
  return(h)
  ### an object of class \code{histogram}, see \code{\link[graphics]{hist}}
},ex=function() {
  histdn(rnorm(100))
})
