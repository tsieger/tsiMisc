\name{locapply}
\alias{locapply}
\title{Apply function to data locally.}
\description{\code{\link{locapply}} applies given function to portions of \code{y}
in the local \code{x} neighbourhood. This generalizes e.g. \code{lowess}
as you can compute any function of \code{y}, not only the mean.}
\usage{locapply(x, y, fun, bandwidth = 0.2, 
    ...)}
\arguments{
  \item{x}{vector of x-coordinates}
  \item{y}{vector of y-coordinates}
  \item{fun}{function to be applied; it will be passed a vector of \code{y} coordinates
in the neighbourhood of some \code{x}}
  \item{bandwidth}{\code{bandwidth} defining the neighbourhood in x-coordinates}
  \item{\dots}{additional arguments to \code{fun}}
}

\value{A list of vectors \code{x} and \code{y} holding the x-coordinates,
and the result of \code{fun} applied to the local neighbourhood
of corresponding element of \code{x}.}

\author{Tomas Sieger}




\examples{
set.seed(1)
n<-100
x<-sort(runif(n,0,5))
y<-runif(n,-exp(x),exp(x))
plot(x,y,pch=19)
lines(x,locapply(x,y,quantile,probs=.5)$y)
lines(x,locapply(x,y,quantile,probs=.9)$y)
lines(x,locapply(x,y,quantile,probs=.1)$y)
lines(x,locapply(x,y,quantile,probs=1)$y)
lines(x,locapply(x,y,quantile,probs=0)$y)
}
