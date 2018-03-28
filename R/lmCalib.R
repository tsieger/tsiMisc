lmCalib<-structure(
function# Calibration in regression.
##description<<
## 'lmCalib' computes the estimate of \eqn{x_0} in which the regression
## of \eqn{y ~ x} reaches given \eqn{y0}, and the related confidence interval.
##
##details<<
## The code is taken from Appendix A.4.5 in Zvara2008.
## The estimation is based on chapter 4.4. in Zvara2008
##
##references<< Karel Zv\'{a}ra: Regrese, Matfyzpress Praha 2008,
## J.A. Neter, W. Wasserman, M. Kutner: Applied Linear Statistical
## Models, Irwin, Homewood, Illinois (1985).
##
##seealso<< 'lmInt'
(x, ##<< independent variable
y, ##<< dependent variable
y0, ##<< dependent variable for which to compute the expected
## independent variable
fixed = FALSE, ##<< is 'y0' fixed, or random?
approx = FALSE, ##<< use approximation, or Fieller method?
alpha = 0.05 ##<< confidence interval significance level
) {
  i <- complete.cases(x, y)
  x <- x[i]
  y <- y[i]
  b1 <- coef(a <- lm(y~x))[2]
  S2 <- deviance(a)/a$df.residual
  n <- length(x)
  x.bar <- mean(x)
  y.bar <- mean(y)
  Txx <- sum((x - x.bar)^2)
  t2 <- qt(1 - alpha/2, n-2)^2
  x.Hat <- x.bar + (y0 - y.bar)/b1
  if (!approx){
    A <- b1^2 - S2*t2/Txx
    B <- -2*b1*(y0 - y.bar)
    C <- (y0 - y.bar)^2 - S2*t2*((fixed==FALSE)+1/n)
    if (A > 0){
      diskr.sqrt <- sqrt(B^2 - 4*A*C)
      xL <- x.bar + (-B - diskr.sqrt)/2/A
      xU <- x.bar + (-B + diskr.sqrt)/2/A      
    }else{
      xL <- -Inf
      xU <- Inf
    }  
  }else{
    xL <- x.Hat - sqrt(S2*t2*((fixed==FALSE) + 1/n + (x.Hat - x.bar)^2/Txx))/abs(b1)
    xU <- x.Hat + sqrt(S2*t2*((fixed==FALSE) + 1/n + (x.Hat - x.bar)^2/Txx))/abs(b1)
  }

  RET <- c(x.Hat, xL, xU)
  names(RET) <- c("x0.hat", "x0.lower", "x0.upper")
  return(RET)  
  ### A vector of the estimate of 'x0', and lower and upper limits of
  ### related confidence interval.
},ex=function() {
  iris.setosa<-iris[iris$Species=='setosa',]
  attach(iris.setosa)

  plot(Sepal.Length, Sepal.Width, xlim=c(min(3.5,min(Sepal.Length)),max(Sepal.Length)))
  abline(coef(lm(Sepal.Width ~ Sepal.Length)))

  y0 <- 3
  abline(h = y0, col='gray')

  # Fieller method with y0 fixed
  calib <- lmCalib(Sepal.Length, Sepal.Width, y0, fixed = TRUE, approx = TRUE)
  lines(calib[2:3],rep(y0,2)*1.01,col='red',lwd=3)
  points(calib[1],y0*1.01,pch=19,cex=1.3,col='red')
  # Fieller method with y0 random
  calib <- lmCalib(Sepal.Length, Sepal.Width, y0, fixed = FALSE, approx = TRUE)
  lines(calib[2:3],rep(y0,2)*1.03,col='red',lwd=1)
  points(calib[1],y0*1.03,pch=19,cex=.8,col='red')

  # approximate method with y0 fixed
  calib <- lmCalib(Sepal.Length, Sepal.Width, y0, fixed = TRUE, approx = FALSE)
  lines(calib[2:3],rep(y0,2)/1.01,lwd=3,col='blue')
  points(calib[1],y0/1.01,pch=19,cex=1.2,col='blue')
  # approximate method with y0 random
  calib <- lmCalib(Sepal.Length, Sepal.Width, y0, fixed = FALSE, approx = FALSE)
  lines(calib[2:3],rep(y0,2)/1.03,lwd=1,col='blue')
  points(calib[1],y0/1.03,pch=19,cex=.8,col='blue')

  legend('topleft', bg='white',
    c(paste0('Fieller method with y0=',y0,' fixed'),
      paste0('Fieller method with y0=',y0,' random'),
      paste0('approximate m. with y0=',y0,' fixed'),
      paste0('approximate m. with y0=',y0,'  random'),
      '(vertical offsets for visualization',
      ' purposes only)'),
    col = c('red', 'red', 'blue', 'blue', 'black'),
    lwd = c(3, 1, 3, 1, NA, NA))

  detach(iris.setosa)
})
