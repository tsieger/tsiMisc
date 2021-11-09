histldn<-structure(
function # Histogram with log counts, defaults, and normal density approximation.
##description<<
## \code{\link{histld}} plots a histogram having the counts logged and
## passing some default values to \code{\link[graphics]{hist}}. A Gaussian
## curve of normal approximation gets superposed onto the histogram.
(x, ##<< a vector of values for which the histogram is desired
breaks = 100, ##<< histogram breaks, see \code{\link[graphics]{hist}}
col = 'gray', ##<< a color to fill the bars, defaults to gray, see \code{\link[graphics]{hist}}
coln = 'red', ##<< a color of normal density estimate
main = NULL, ##<<
xlab = NULL, ##<<
plot = TRUE, ##<<
... ##<< other parameters to \code{\link[graphics]{hist}}
) {
  if (is.null(main)) main<-paste('Histogram of',deparse(substitute(x)))
  if (is.null(xlab)) xlab<-deparse(substitute(x))
  h<-hist(x,breaks=breaks,plot=FALSE)
  h$counts<-log10(1+h$counts)
  if (plot) {
      # TODO: accept 'freq' argument?
      plot(h,col=col,main=main,xlab=xlab,ylab='Log Frequency',...)
      mnx<-sum(h$counts*h$mids)/sum(h$counts)
      sdx<-sqrt(sum(h$counts*(h$mids-mnx)^2)/sum(h$counts))
      y<-dnorm(h$mids,mnx,sdx)*diff(h$mids[1:2])*sum(h$counts)
      lines(h$mids,y,col=coln)
  }
  return(invisible(h))
  ### an object of class \code{histogram}, see \code{\link[graphics]{hist}}
},ex=function() {
  opar<-par(mfrow=c(1,2))
  x<-rnorm(1000)
  x<-c(x,rep(x[abs(x)<1],10),rep(x[abs(x)<.3],20),rep(x[abs(x)<.1],50))
  histdn(x)
  histldn(x)
  par(opar)
})
