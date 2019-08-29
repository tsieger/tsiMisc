\name{percentTrue}
\alias{percentTrue}
\title{Percentual ratio of TRUE cases.}
\description{\code{\link{percentTrue}} computes the percentual ratio of
\code{TRUE} cases}
\usage{percentTrue(x, digits = NULL)}
\arguments{
  \item{x}{a logical vector (or a vector that can be coerced to one)}
  \item{digits}{number of decimal places (passed to \code{round})}
}

\value{Percentual ratio of \code{TRUE} cases of \code{x}.}

\author{Tomas Sieger <tomas.sieger@seznam.cz> [aut, cre]}




\examples{
percentTrue(c(TRUE, TRUE, TRUE))
percentTrue(c(TRUE, FALSE, FALSE), digits = 1)
percentTrue(c(FALSE))
percentTrue(NULL)
}
