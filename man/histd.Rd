\name{histd}
\alias{histd}
\title{Histogram with defaults.}
\description{'histd' is just a shortcut to 'hist' passing it some default values.}
\usage{histd(x, breaks = 100, col = "gray", main = NULL, xlab = NULL, 
    ...)}
\arguments{
  \item{x}{a vector of values for which the histogram is desired}
  \item{breaks}{histogram breaks, see 'hist'}
  \item{col}{a color to fill the bars, defaults to gray, see 'hist'}
  \item{main}{
}
  \item{xlab}{
}
  \item{\dots}{other parameters to 'hist'}
}

\value{an object of class 'histogram', see 'hist'}

\author{Tomas Sieger <tomas.sieger@seznam.cz> [aut, cre]}




\examples{
histd(rnorm(100))
}
