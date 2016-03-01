dagostinoTest<-structure(
function # D'Agostino normality test.
##description<<
## Performs D'Agostino normality tests based on skewness, kurtosis, and
## an omnibus test.
##
##details<<
## The implementation is taken from Zvara2008 (appendix A.4.1).
##
##references<< DAgostino, Ralph B.; Albert Belanger; Ralph B. DAgostino, Jr
## (1990). A suggestion for using powerful and informative tests of normality.
## The American Statistician 44 (4): 316-321. JSTOR 2684359.
##
## Karel Zv\'{a}ra: Regrese, Matfyzpress Praha 2008
(x ##<< a non-empty numeric vector of data
)
{
  DNAME <- deparse(substitute(x))
  x <- x[complete.cases(x)]
  n <- length(x)
  if (n<6) stop("sample size must be at least 6")
  meanX <- mean(x)
  s<- sqrt(mean((x-meanX)**2))
  a3 <- mean((x-meanX)**3)/s**3
  a4 <- mean((x-meanX)**4)/s**4
  SD3 <- sqrt(6*(n-2)/((n+1)*(n+3)))
  SD4 <- sqrt(24*(n-2)*(n-3)*n/((n+1)**2*(n+3)*(n+5)))
  U3 <- a3/SD3
  U4 <- (a4-3+6/(n+1))/SD4
  b <-(3*(n**2+27*n-70)*(n+1)*(n+3))/((n-2)*(n+5)*(n+7)*(n+9))
  W2 <- sqrt(2*(b-1))-1
  delta <- 1/sqrt(log(sqrt(W2)))
  a <- sqrt(2/(W2-1))
  Z3 <- delta*log((U3/a)+sqrt((U3/a)**2+1))
  B <- (6*(n*n-5*n+2)/((n+7)*(n+9)))*sqrt((6*(n+3)*(n+5))/(n*(n-2)*(n-3)))  
  A <- 6+(8/B)*((2/B)+sqrt(1+4/(B**2)))
  jm <- sqrt(2/(9*A))
  pos <- ((1-2/A)/(1+U4*sqrt(2/(A-4))))**(1/3)
  Z4 <- (1-2/(9*A)-pos)/jm
  omni <- Z3**2+Z4**2
  pZ3 <- 2*(1-pnorm(abs(Z3),0,1))
  pZ4 <- 2*(1-pnorm(abs(Z4),0,1))
  pomni <- 1-pchisq(omni,2)
  skewness <- c(Z3,pZ3)
  kurtosis <- c(Z4,pZ4)
  omnibus <- c(omni,pomni)
  DA <- cbind(skewness,kurtosis,omnibus)
  row.names(DA)<-c("statistics","p-value")
  return(DA)
  ### Data frame having the results of the skewness, kurtosis, and
  ### omnibus tests in columns, and the test statistics and P-values
  ### in rows.
},ex=function() {
  dagostinoTest(rnorm(100, mean = 5, sd = 3))
  dagostinoTest(runif(100, min = 2, max = 4))
})
