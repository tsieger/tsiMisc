\name{cartesian2polar}
\alias{cartesian2polar}
\title{Cartesian to polar coordinate conversion.}
\description{Cartesian to polar coordinate conversion.
}
\usage{cartesian2polar(x, y = NULL)}
\arguments{
  \item{x}{vector of \code{x} coordinates (or a vector of length 2 holding
\eqn{x, y}, or a matrix of two columns holding \code{x} and \code{y} coordinates)}
  \item{y}{vector \code{y} coordinates}
}

\value{A matrix of the \eqn{r} and \eqn{phi} polar coordinates in columns.}

\author{Tomas Sieger}



\seealso{\code{\link{cartesian2spherical}}, \code{\link{polar2cartesian}}}
\examples{
cartesian2polar(0, 0)
cartesian2polar(1, 2)
cartesian2polar(c(1, 2))
cartesian2polar(cbind(1:10,1:10))
}
