expit<-structure(
function # The expit function (the inverse to logit).
##<< details
## 'expit' computes 'exp(x) / (1 + exp(x))', i.e. the inverse to
## 'logit', i.e. it maps the interval '(-inf, inf)' to the interval
## of '[0, 1]'.
##<<seealso logit
(x ##<< a number
) {
  return(exp(x)/(1+exp(x)))
  ### expit of x
},ex=function() {
  expit(-1e10)
  expit(0)
  expit(1e10)
})
