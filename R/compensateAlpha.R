compensateAlpha<-structure(
function # Significance level compensation for multiple comparisons.
##description<<
## \code{\link{compensateAlpha}} compensates statistical significance level
## (alpha) for multiple comparisons/tests.
##
##references<<
## SISA, \url{http://www.quantitativeskills.com/sisa/calculations/bonhlp.htm}
##seealso<< \code{\link{compensatePValue}}, \code{\link[stats]{p.adjust}}
(alpha, ##<< significance level to be compensated
n, ##<< comparison count
method = c('bonferroni', 'sidak'), ##<< compensation method,
## 'bonferroni' is more conservative, but does not assume
## independence of the tests,
## 'sidak' is more liberal and assumes independence of the tests
r = 0 ##<< optional mean "correlation between the tests made",
## the extreme value of 0 leads to full compensation, the extreme value
## of 1 leads to no compensation (as all the tests are considered equal).
## See the SISA help for explanation.
) {
  if (r<0 || r>1) stop('correlation must be in range 0..1')
  # see http://www.quantitativeskills.com/sisa/calculations/bonfer.htm
  n<-n^(1-r)

  method<-match.arg(method)
  if (method=='sidak') {
    alpha<-(1-(1-alpha)^(1/n))
  }
  else if (method=='bonferroni') {
    alpha<-alpha/n
  } else stop('unsupported method "',method,'"')
  return(alpha)
  ### Compensated significance level. It equals \code{alpha/n} for the
  ### \code{'bonferroni'} method, and \code{1-(1-alpha)^(1/n))} for the \code{'sidak'}
  ### method.
},ex=function() {
  compensateAlpha(.05,2,method='sidak')
})
