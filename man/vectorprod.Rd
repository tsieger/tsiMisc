\name{vectorprod}
\alias{vectorprod}
\title{3D vector product.}
\description{\code{\link{vectorprod}} computes 3D vector product.}
\usage{vectorprod(a, b)}
\arguments{
  \item{a}{first vector}
  \item{b}{second vector}
}

\value{Vector product of 'a' and 'b'.}

\author{Tomas Sieger}




\examples{
vectorprod(c(1,0,0),c(0,1,0))
crossprod(c(1,0,0),c(0,1,0))

vectorprod(c(1,0,0),c(1,0,0))
crossprod(c(1,0,0),c(1,0,0))
}
