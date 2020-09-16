\name{histd}
\alias{histd}
\title{Histogram with defaults.}
\description{\code{\link{histd}} is just a shortcut to \code{\link[graphics]{hist}}
passing it some default values.}
\usage{histd(x, breaks = 100, 
    col = "gray", main = NULL, 
    xlab = NULL, ...)}
\arguments{
  \item{x}{a vector of values for which the histogram is desired}
  \item{breaks}{histogram \code{breaks}, see \code{\link[graphics]{hist}}}
  \item{col}{a color to fill the bars, defaults to gray, see \code{\link[graphics]{hist}}}
  \item{main}{
}
  \item{xlab}{
}
  \item{\dots}{other parameters to \code{\link[graphics]{hist}}}
}

\value{an object of class \code{histogram}, see \code{\link[graphics]{hist}}}

\author{Tomas Sieger}




\examples{
histd(rnorm(100))
}
