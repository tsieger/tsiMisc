\name{normalizeVector}
\alias{normalizeVector}
\title{Normalize vector to unit length.}
\description{\code{normalizeVector} normalizes a vector to unit length.}
\usage{normalizeVector(v)}
\arguments{
  \item{v}{a vector}
}

\value{normalized vector}

\author{Tomas Sieger}




\examples{
normalizeVector(c(1, 0, 0))
normalizeVector(c(1, 1, 0))
normalizeVector(c(1, 1, 1))
normalizeVector(c(-1, 2, 3))
vectorLength(normalizeVector(c(-1, 2, 3)))
}
