\name{lmCalib}
\alias{lmCalib}
\title{Calibration in regression.}
\description{\code{\link{lmCalib}} computes the estimate of \eqn{x_0} in which the regression
of \eqn{y ~ x} reaches given \eqn{y0}, and the related confidence interval.
}
\usage{lmCalib(x, y, y0, fixed = FALSE, approx = FALSE, alpha = 0.05)}
\arguments{
  \item{x}{independent variable}
  \item{y}{dependent variable}
  \item{y0}{dependent variable for which to compute the expected
independent variable}
  \item{fixed}{is \code{y0} fixed, or random?}
  \item{approx}{use approximation, or Fieller method?}
  \item{alpha}{confidence interval significance level}
}
\details{The code is taken from Appendix A.4.5 in Zvara2008.
The estimation is based on chapter 4.4. in Zvara2008
}
\value{A vector of the estimate of 'x0', and lower and upper limits of
related confidence interval.}
\references{Karel Zv\'{a}ra: Regrese, Matfyzpress Praha 2008,
J.A. Neter, W. Wasserman, M. Kutner: Applied Linear Statistical
Models, Irwin, Homewood, Illinois (1985).
}
\author{Tomas Sieger <tomas.sieger@seznam.cz> [aut, cre]}



\seealso{\code{\link{lmInt}}}
\examples{
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
}
