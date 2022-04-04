\name{polar2cartesian}
\alias{polar2cartesian}
\title{Polar to Cartesian coordinate conversion.}
\description{\code{\link{polar2cartesian}} provides polar to Cartesian
coordinate conversion.
}
\usage{polar2cartesian(r, phi = NULL)}
\arguments{
  \item{r}{a vector of radii (a or a vector of length 2 holding \eqn{r, phi},
or a matrix of two columns holding \eqn{r} and \eqn{phi} coordinates)}
  \item{phi}{a vector of angles}
}

\value{A matrix of the \eqn{x} and \eqn{y} Cartesian coordinates in columns.}

\author{Tomas Sieger}



\seealso{\code{\link{cartesian2polar}}}
\examples{
polar2cartesian(1, 0)
polar2cartesian(1, pi)
polar2cartesian(c(1, pi))
polar2cartesian(cbind(1, seq(0,pi,.1)))
}
