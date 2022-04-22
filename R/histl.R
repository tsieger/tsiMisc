histl<-structure(
function # Histogram with log counts.
##description<<
## \code{\link{histld}} plots a histogram having the counts logged.
(x, ##<< a vector of values for which the histogram is desired
main = NULL, ##<<
xlab = NULL, ##<<
plot = TRUE, ##<< produce a plot?
... ##<< other parameters to \code{\link[graphics]{hist}}
) {
  if (is.null(main)) main<-paste('Histogram of',deparse(substitute(x)))
  if (is.null(xlab)) xlab<-deparse(substitute(x))
  args<-list(...)
  hargs<-list(x=x,plot=FALSE)
  if (!is.null(args)) {
      hargs<-c(hargs,args)
  }
  h<-do.call('hist',hargs)
  h$counts<-log10(1+h$counts)
  if (plot) {
      # TODO: accept 'freq' argument?
      hargs<-list(x=h,main=main,xlab=xlab,ylab='Log Frequency')
      if (!is.null(names(args))) {
        hargs<-c(hargs,args[!'breaks'%in%names(args)])
      }
      do.call('plot',hargs)
  }
  return(invisible(h))
  ### an object of class \code{histogram}, see \code{\link[graphics]{hist}}
},ex=function() {
  opar<-par(mfrow=c(1,2))
  x<-rexp(1000)
  hist(x)
  histl(x)
  par(opar)
})
