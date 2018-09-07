histd<-structure(
function # Histogram with defaults.
##description<<
## 'histd' is just a shortcut to 'hist' passing it some default values.
(x, ##<< a vector of values for which the histogram is desired
breaks = 100, ##<< histogram breaks, see 'hist'
col = 'gray', ##<< a color to fill the bars, defaults to gray, see 'hist'
main = NULL, ##<<
xlab = NULL, ##<<
... ##<< other parameters to 'hist'
) {
  if (is.null(main)) main<-paste('Histogram of',deparse(substitute(x)))
  if (is.null(xlab)) xlab<-deparse(substitute(x))
  hist(x,breaks=breaks,col=col,main=main,xlab=xlab,...)
  ### an object of class 'histogram', see 'hist'
},ex=function() {
  histd(rnorm(100))
})
