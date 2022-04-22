\name{histl}
\alias{histl}
\title{Histogram with log counts.}
\description{\code{\link{histld}} plots a histogram having the counts logged.}
\usage{histl(x, main = NULL, 
    xlab = NULL, plot = TRUE, 
    ...)}
\arguments{
  \item{x}{a vector of values for which the histogram is desired}
  \item{main}{
}
  \item{xlab}{
}
  \item{plot}{produce a \code{plot}?}
  \item{\dots}{other parameters to \code{\link[graphics]{hist}}}
}

\value{an object of class \code{histogram}, see \code{\link[graphics]{hist}}}

\author{Tomas Sieger}




\examples{
opar<-par(mfrow=c(1,2))
x<-rexp(1000)
hist(x)
histl(x)
par(opar)
}
