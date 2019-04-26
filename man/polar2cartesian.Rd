\name{polar2cartesian}
\alias{polar2cartesian}
\title{Polar to Cartesian coordinate conversion.}
\description{'polar2cartesian' provides polar to Cartesian coordinate conversion.
}
\usage{polar2cartesian(r, phi = NULL)}
\arguments{
  \item{r}{a vector of radii (a or a vector of length 2 holding \eqn{r, phi})}
  \item{phi}{a vector of angles}
}

\value{A list of the \eqn{x} and \eqn{y} Cartesian coordinates.}

\author{Tomas Sieger <tomas.sieger@seznam.cz> [aut, cre]}



\seealso{'cartesian2polar'}
\examples{
polar2cartesian(1, 0)
polar2cartesian(1, pi)
polar2cartesian(c(1, pi))
}
