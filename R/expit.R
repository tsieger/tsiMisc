expit<-structure(
function # The expit function (the inverse to logit).
##description<<
## \code{\link{expit}} computes \eqn{exp(x) / (1 + exp(x))}, i.e. the inverse to
## \code{\link{logit}}, i.e. it maps the interval \eqn{(-\infty, \infty)}
## to the interval of \eqn{[0, 1]}.
##
##seealso<< 'logit'
(x ##<< a number
) {
  return(exp(x)/(1+exp(x)))
  ### expit of x
},ex=function() {
  expit(-1e10)
  expit(0)
  expit(1e10)
})
