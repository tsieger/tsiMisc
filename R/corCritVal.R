corCritVal<-structure(
function # Critical values for correlation coefficients.
##<< details
## TODO: details, reference
(n, ##<< sample size
alpha, ##<< significance level
method = c('pearson', 'spearman'),
approximate = FALSE
) {
  method<-match.arg(method)
  if (method=='pearson') {
    if (approximate) {
        v<-qnorm(1-alpha/2,0,1/sqrt(n-2))
    } else {
      # a = r/sqrt(1-r^2)
      # r = +- sqrt(1/(1+1/a^2))
      a<-qt(1-alpha/2,n-2)/sqrt(n-2)
      v<-sqrt(1/(1+1/a^2))
    }
  } else if (method=='spearman') {
    stop(paste0('TODO: unimplemented for method \'',method,'\''))
  } else {
    stop(paste0('unsupported method \'',method,'\''))
  }
  return(v)
  ### 
},ex=function() {
  corCritVal(20)
})
