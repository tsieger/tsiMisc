\name{spherical2cartesian}
\alias{spherical2cartesian}
\title{Spherical to Cartesian coordinate conversion.}
\description{'spherical2cartesian' provides spherical to Cartesian coordinate
conversion.
}
\usage{spherical2cartesian(r, theta = NULL, phi = NULL)}
\arguments{
  \item{r}{radius (or a vector of length 3 holding \eqn{r, theta,
phi})}
  \item{theta}{inclination}
  \item{phi}{azimuth}
}

\value{A vector of three Cartesian coordinates (\eqn{x, y, z}) of a
point represented as (\eqn{r, theta, phi}) in spherical
coordinates.}

\author{Tomas Sieger <tomas.sieger@seznam.cz> [aut, cre]}



\seealso{'cartesian2spherical'}
\examples{
spherical2cartesian(c(1, 0, pi / 2))
spherical2cartesian(c(1, pi, pi / 4))
}
