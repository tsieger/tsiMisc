logit<-structure(
function # The logit function.
##description<<
## \code{\link{logit}} computes \eqn{log( p / (1-p))}, i.e. maps the interval \eqn{[0, 1]}
## to the interval of \eqn{(-\infty, \infty)}.
##
##seealso<< \code{\link{expit}}
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
