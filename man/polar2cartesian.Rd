\name{polar2cartesian}
\alias{polar2cartesian}
\title{Polar to Cartesian coordinate conversion.}

\usage{polar2cartesian(r, phi = NULL)}
\arguments{
  \item{r}{radius (or a vector of length 2 holding \eqn{r, phi})}
  \item{phi}{angle}
}

\value{A vector of two Cartesian coordinates (\eqn{x, y}) of a
point represented as (\eqn{r, phi}) in polar
coordinates.}

\author{Tomas Sieger}




\examples{
polar2cartesian(c(1, 0))
polar2cartesian(c(1, pi))
}
