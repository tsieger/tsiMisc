\name{histdn}
\alias{histdn}
\title{Histogram with defaults and normal density approximation.}
\description{\code{\link{histd}} is just a shortcut to \code{\link[graphics]{hist}}
passing it some default values, with a Gaussian curve of normal approximation
superposed onto it.}
\usage{histdn(x, breaks = 100, 
    col = "gray", coln = "red", 
    main = NULL, xlab = NULL, 
    ...)}
\arguments{
  \item{x}{a vector of values for which the histogram is desired}
  \item{breaks}{histogram \code{breaks}, see \code{\link[graphics]{hist}}}
  \item{col}{a color to fill the bars, defaults to gray, see \code{\link[graphics]{hist}}}
  \item{coln}{a color of normal density estimate}
  \item{main}{
}
  \item{xlab}{
}
  \item{\dots}{other parameters to \code{\link[graphics]{hist}}}
}

\value{an object of class \code{histogram}, see \code{\link[graphics]{hist}}}

\author{Tomas Sieger}




\examples{
histdn(rnorm(100))
}
