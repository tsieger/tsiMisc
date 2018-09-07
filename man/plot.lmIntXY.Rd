\name{plot.lmIntXY}
\alias{plot.lmIntXY}
\title{Plot confidence and prediction intervals for regression.}
\description{'plot.lmIntXY' adds confidence and/or prediction intervals to
an existing plot depicting a simple univariate linear model.
The intervals are computed using the 'lmInt' function.
}
\usage{\method{plot}{lmIntXY}(x, fit = FALSE, cia = FALSE, cif = FALSE, pi = FALSE, 
    ...)}
\arguments{
  \item{x}{an object of class 'lmIntXY'}
  \item{fit}{plot the fitted regression line?}
  \item{cia}{plot confidence intervals \emph{around} the regression
line? (See 'lmInt' for explanation.)}
  \item{cif}{plot confidence intervals \emph{for} the regression
line? (See 'lmInt' for explanation.)}
  \item{pi}{plot prediction intervals? (See 'lmInt' for
explanation.)}
  \item{\dots}{additional arguments to be passed to the 'lines' function used
to plot the lines}
}



\author{Tomas Sieger <tomas.sieger@seznam.cz> [aut, cre]}
\note{deprecated, use 'lmInt' and 'plot.lmInt' instead
}


\seealso{'lmIntXY'
}
\examples{
# see lmIntXY
}
