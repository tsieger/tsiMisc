lmSignif<-structure(
function # Overall linear model significance.
##description<<
## 'lmSignif' compute the significance of the F statistics in a
## 'lm.summary' object.
(s ##<< an object of class 'lm.summary'
## (or a linear model of class 'lm' that will get summarized)
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
