\name{plot.lmInt}
\alias{plot.lmInt}
\title{Plot confidence and prediction intervals for regression.}
\description{'plot.lmInt' adds confidence and/or prediction intervals to
an existing plot depicting a simple univariate linear model.
The intervals are computed using the 'lmInt' function.
}
\usage{\method{plot}{lmInt}(x, xx = colnames(x)[1], fit = FALSE, cia = FALSE, 
    cif = FALSE, pi = FALSE, band = FALSE, ...)}
\arguments{
  \item{x}{an object of class 'lmInt'}
  \item{xx}{the name of dependent variable to plot against}
  \item{fit}{plot the fitted regression line?}
  \item{cia}{plot confidence intervals \emph{around} the regression
line? (See 'lmInt' for explanation.)}
  \item{cif}{plot confidence intervals \emph{for} the regression
line? (See 'lmInt' for explanation.)}
  \item{pi}{plot prediction intervals? (See 'lmInt' for
explanation.)}
  \item{band}{draw polygonal confidence bands instead of simple lines?}
  \item{\dots}{additional arguments to be passed to the 'lines' function used
to plot the lines}
}



\author{Tomas Sieger <tomas.sieger@seznam.cz> [aut, cre]}



\seealso{'lmInt'
}
\examples{
# see lmInt
}
