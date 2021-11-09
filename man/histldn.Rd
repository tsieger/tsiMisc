\name{histldn}
\alias{histldn}
\title{Histogram with log counts, defaults, and normal density approximation.}
\description{\code{\link{histld}} plots a histogram having the counts logged and
passing some default values to \code{\link[graphics]{hist}}. A Gaussian
curve of normal approximation gets superposed onto the histogram.}
\usage{histldn(x, breaks = 100, 
    col = "gray", coln = "red", 
    main = NULL, xlab = NULL, 
    plot = TRUE, ...)}
\arguments{
  \item{x}{a vector of values for which the histogram is desired}
  \item{breaks}{histogram \code{breaks}, see \code{\link[graphics]{hist}}}
  \item{col}{a color to fill the bars, defaults to gray, see \code{\link[graphics]{hist}}}
  \item{coln}{a color of normal density estimate}
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
x<-rnorm(1000)
x<-c(x,rep(x[abs(x)<1],10),rep(x[abs(x)<.3],20),rep(x[abs(x)<.1],50))
histdn(x)
histldn(x)
par(opar)
}
