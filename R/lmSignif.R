lmSignif<-structure(
function # Overall linear model significance.
##description<<
## \code{\link{lmSignif}} compute the significance of the F statistics in a
## \code{\link[stats]{summary.lm}} object.
(s ##<< an object of class \code{\link[stats]{summary.lm}}
## (or a linear model of class \code{\link[stats]{lm}} that will get summarized)
) {
  if (!inherits(s,'summary.lm')) {
    if (inherits(s,'lm')) {
      s<-summary(s)
    } else {
      stop('need a \'summary.lm\' object')
    }
  }
  if (!is.null(s$fstatistic)) {
    p<-(1-pf(s$fstatistic['value'],s$fstatistic['numdf'],s$fstatistic['dendf']))
  } else {
    p<-NA
  }
  return(p)
  ### Overall significance of a linear model.
},ex=function() {
  m <- lm(Sepal.Length~Sepal.Width, data=iris[iris$Species=='setosa',])
  summary(m)
  lmSignif(summary(m))
})
