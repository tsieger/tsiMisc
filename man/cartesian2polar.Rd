\name{cartesian2polar}
\alias{cartesian2polar}
\title{Cartesian to polar coordinate conversion.}
\description{Cartesian to polar coordinate conversion.
}
\usage{cartesian2polar(x, y = NULL)}
\arguments{
  \item{x}{\code{x} coordinate (or a vector of length 2 holding \eqn{x, y})}
  \item{y}{\code{y} coordinate}
}

\value{A list of the \eqn{r} and \eqn{phi} polar coordinates.}

\author{Tomas Sieger}



\seealso{\code{\link{cartesian2spherical}}, \code{\link{polar2cartesian}}}
\examples{
cartesian2polar(0, 0)
cartesian2polar(1, 2)
cartesian2polar(c(1, 2))
}
