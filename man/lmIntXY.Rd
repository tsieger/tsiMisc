\name{lmIntXY}
\alias{lmIntXY}
\title{Confidence and prediction intervals for regression.}
\description{\code{\link{lmInt}} contructs confidence and prediction intervals for simple
linear regression. For a simple univariate linear model in the form
of \eqn{EY = beta_0 + beta_1 X} computed using 'lm(\code{y} ~ \code{x})',
'lmIntervals' computes confidence intervals around the regression
line (i.e. the point-wise confidence bands of \eqn{E(Y | X = \code{x})}
for each individual \eqn{x}), confidence intervals for the
regression line (i.e. the simultaneous confidence bands of
\eqn{E(Y)} for all \eqn{x}), and prediction intervals (i.e.
point-wise confidence bands for new observations \eqn{Y} for each
individual \eqn{x}).
}
\usage{lmIntXY(x, y, d = 100)}
\arguments{
  \item{x}{independent variable, or a fitted linear model}
  \item{y}{dependent variable}
  \item{d}{a vector of values to compute the intervals for, or a number of values
to be automatically generated to uniformly span the range of `x'}
}
\details{The confidence intervals around the regression line and the
prediction intervals are computed using the 'predict.lm' function.
The confidence intervals for the regression line are computed
according to eq. (4.15) in Zvara2008.
}
\value{An object of class \code{lmInt} - a data frame of columns
\code{x} holding the values of the independent variable the intervals
are computed for,
\code{fit} holding the mean value fitted by the regression model,
\code{ciaLwr} and \code{ciaUpr} holding confidence intervals
\emph{a}round the regression line,
\code{cifLwr} and \code{cifUpr} holding confidence intervals
\emph{f}or the regression line, and
\code{piLwr} and \code{piUpr} holding predictions intervals.}
\references{Karel Zv\'{a}ra: Regrese, Matfyzpress Praha 2008
}
\author{Tomas Sieger}
\note{deprecated, use \code{\link{lmInt}} instead
}


\seealso{\code{\link[stats]{predict.lm}}, \code{\link[stats]{lm}}
}
\examples{
iris.setosa<-iris[iris$Species=='setosa',]
attach(iris.setosa)
lmi <- lmIntXY(Sepal.Length, Sepal.Width, 30)
plot(Sepal.Length, Sepal.Width)
plot(lmi, fit = TRUE, lty = 1, col='red')
plot(lmi, cia = TRUE, lty = 1)
plot(lmi, cif = TRUE, lty = 2)
plot(lmi, pi = TRUE, lty = 3)
legend('topright', bg='white',
  c('fitted regression line',
  'confidence interval around the regression line',
  'confidence interval for the regression line',
  'prediction int.'),
  col = c('red', 'black', 'black', 'black'), lty = c(1, 1, 2, 3))
detach(iris.setosa)
}
