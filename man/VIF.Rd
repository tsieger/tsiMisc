\name{VIF}
\alias{VIF}
\title{Variance-inflaction factor.}
\description{\code{\link{VIF}} computes multicolinearity-related diagnostic
statistics for a linear model with an absolute term.
}
\usage{VIF(m)}
\arguments{
  \item{m}{linear model fitted by 'lm'}
}
\details{The implementation was taken from Zvara2008.
}
\value{a matrix. In rows: diagnostic statistics related to individual
regressors in the original model.
In columns: regression coefficient estimate in a standardized
model, variance inflation factor, the coefficient of
determination R^2 in model explaining the current regressor using
all the others, and tolerance (1-R^2 = 1/VIF).}
\references{Karel Zvara: Regrese, Matfyzpress Praha 2008
}
\author{Tomas Sieger <tomas.sieger@seznam.cz> [aut, cre]}



\seealso{\code{\link[car]{vif}}}
\examples{
require(stats)
require(datasets)
m<-lm(Sepal.Width~Sepal.Length+Petal.Width+Petal.Length,iris)
VIF(m)
}
