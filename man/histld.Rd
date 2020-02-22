\name{histld}
\alias{histld}
\title{Histogram with log counts and defaults.}
\description{\code{\link{histld}} plots a histogram having the counts logged and
passing some default values to \code{\link[graphics]{hist}}.}
\usage{histld(x, breaks = 100, col = "gray", main = NULL, xlab = NULL, 
    plot = TRUE, ...)}
\arguments{
  \item{x}{a vector of values for which the histogram is desired}
  \item{breaks}{histogram breaks, see \code{\link[graphics]{hist}}}
  \item{col}{a color to fill the bars, defaults to gray, see \code{\link[graphics]{hist}}}
  \item{main}{
}
  \item{xlab}{
}
  \item{plot}{
}
  \item{\dots}{other parameters to \code{\link[graphics]{hist}}}
}

\value{an object of class \code{histogram}, see \code{\link[graphics]{hist}}}

\author{Tomas Sieger}




\examples{
opar<-par(mfrow=c(1,2))
x<-rexp(1000)
histd(x)
histld(x)
par(opar)
}
