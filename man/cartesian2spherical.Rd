\name{cartesian2spherical}
\alias{cartesian2spherical}
\title{Cartesian to spherical coordinate conversion.}
\description{Cartesian to spherical coordinate conversion.
}
\usage{cartesian2spherical(x, 
    y = NULL, z = NULL)}
\arguments{
  \item{x}{a vector of \code{x} coordinates (or a vector of length 3 holding \eqn{x, \code{y}, z})}
  \item{y}{a vector of \code{y} coordinates}
  \item{z}{
}
}

\value{A vector of three spherical coordinates \eqn{r, theta, phi} of a
point represented as \eqn{x, \code{y}, z} in Cartesian coordinates.}

\author{Tomas Sieger}



\seealso{\code{\link{spherical2cartesian}}}
\examples{
cartesian2spherical(0, 0, 0)
cartesian2spherical(1, 2, 3)
cartesian2spherical(c(1, 2, 3))
}
