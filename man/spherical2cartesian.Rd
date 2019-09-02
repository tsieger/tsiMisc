\name{spherical2cartesian}
\alias{spherical2cartesian}
\title{Spherical to Cartesian coordinate conversion.}
\description{\code{\link{spherical2cartesian}} provides spherical to
Cartesian coordinate conversion.
}
\usage{spherical2cartesian(r, theta = NULL, phi = NULL)}
\arguments{
  \item{r}{a vector of radii (or a vector of length 3 holding \eqn{r, theta,
phi})}
  \item{theta}{a vector of inclinations}
  \item{phi}{a vector of azimuths}
}

\value{A list of \eqn{x}, \eqn{y}, and \eqn{z} Cartesian coordinates.}

\author{Tomas Sieger}



\seealso{\code{\link{cartesian2spherical}}}
\examples{
spherical2cartesian(1, 0, pi / 2)
spherical2cartesian(1, pi, pi / 4)
spherical2cartesian(c(1, pi, pi / 4))
}
