logit<-structure(
function # The logit function.
##<< details
## 'logit' computes 'log( p / (1-p))', i.e. maps the interval '[0, 1]'
## to the interval of '(-inf, inf)'.
##<<seealso expit
(p ##<< a number in the range of <0, 1>
) {
  if (any(p<0) || any(p>1)) {
    stop('invalid \'p\' argument')
  }
  return(log(p/(1-p)))
  ### logit
},ex=function() {
  logit(0)
  logit(.5)
  logit(1)
})
