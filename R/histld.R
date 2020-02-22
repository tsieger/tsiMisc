histld<-structure(
function # Histogram with log counts and defaults.
##description<<
## \code{\link{histld}} plots a histogram having the counts logged and
## passing some default values to \code{\link[graphics]{hist}}.
(x, ##<< a vector of values for which the histogram is desired
breaks = 100, ##<< histogram breaks, see \code{\link[graphics]{hist}}
col = 'gray', ##<< a color to fill the bars, defaults to gray, see \code{\link[graphics]{hist}}
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
  }
  return(invisible(h))
  ### an object of class \code{histogram}, see \code{\link[graphics]{hist}}
},ex=function() {
  opar<-par(mfrow=c(1,2))
  x<-rexp(1000)
  histd(x)
  histld(x)
  par(opar)
})
