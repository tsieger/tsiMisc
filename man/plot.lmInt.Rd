\name{plot.lmInt}
\alias{plot.lmInt}
\title{Plot confidence and prediction intervals for regression.}

\usage{\method{plot}{lmInt}(lmi, x = colnames(lmi)[1], fit = FALSE, cia = FALSE, 
    cif = FALSE, pi = FALSE, band = FALSE, ...)}
\arguments{
  \item{lmi}{an object of class 'lmInt'}
  \item{x}{the name of dependent variable to plot against}
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
\details{'plot.lmInt' adds confidence and/or prediction intervals to
an existing plot depicting a simple univariate linear model.
The intervals are computed using the 'lmInt' function.
}


\author{Tomas Sieger}



\seealso{lmInt
}
\examples{
# see lmInt
}
