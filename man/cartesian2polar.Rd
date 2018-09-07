\name{cartesian2polar}
\alias{cartesian2polar}
\title{Cartesian to polar coordinate conversion.}
\description{Cartesian to polar coordinate conversion.
}
\usage{cartesian2polar(x, y = NULL)}
\arguments{
  \item{x}{x coordinate (or a vector of length 2 holding \eqn{x, y})}
  \item{y}{y coordinate}
}

\value{A vector of two polar coordinates \eqn{r, phi} of a
point represented as \eqn{x, y} in Cartesian coordinates.}

\author{Tomas Sieger <tomas.sieger@seznam.cz> [aut, cre]}



\seealso{'cartesian2spherical', 'polar2cartesian'}
\examples{
cartesian2polar(c(0, 0))
cartesian2polar(c(1, 2))
}
