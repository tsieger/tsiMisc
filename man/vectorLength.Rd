\name{vectorLength}
\alias{vectorLength}
\title{Vector length.}
\description{\code{\link{vectorLength}} computes the length of a vector.}
\usage{vectorLength(v)}
\arguments{
  \item{v}{a vector}
}

\value{vector length}

\author{Tomas Sieger <tomas.sieger@seznam.cz> [aut, cre]}




\examples{
vectorLength(c(1, 0, 0))
vectorLength(c(1, 1, 0))
vectorLength(c(1, 1, 1))
vectorLength(c(-1, 2, 3))
}
