poissonTest<-structure(
function # Test of Poisson distribution.
##description<<
## A simple test of a hypothesis that a given sample 'x' comes from a
## Poisson distribution with unknown parameter. The test is described
## in Zvara (2008), chapter 10.5.
##
##references<< Karel Zv\'{a}ra. _Regrese._ Matfyzpress Praha 2008.
## Chapter 10.5
(x, ##<< a vector of samples
alpha = 0.05, ##<< significance level
na.rm=FALSE ##<< shall NA's be removed first?
) {
  if (na.rm) x<-x[!is.na(x)]
  l<-mean(x)
  if (l<5) {
    warning('The estimate of the rate parameter is less then 5, the test may be inaccurate.')
  }
  q<-crossprod(x-l)/l
  names(q)<-'X-squared'
  q1<-qchisq(1-alpha/2,length(x)-1,lower.tail=FALSE)
  q2<-qchisq(alpha/2,length(x)-1,lower.tail=FALSE)
  p1<-pchisq(q,length(x)-1,lower.tail=FALSE)
  p2<-1-p1

  #return(list(q=q, q1=q1, q2=q2, p=2*min(p1,p2)))
  structure(list(statistic = q, p.value = 2*min(p1,p2),
    method = 'Chi-square test of Poisson distribution',
    data.name = deparse(substitute(x))),
    class = "htest")

  ### a list of 'q' (the test statistics), 'q1, q2' (the critical
  ### values), and 'p' (the p-value of the test)
},ex=function() {
  poissonTest(rpois(20,3))
  poissonTest(abs(rnorm(20)))
})
