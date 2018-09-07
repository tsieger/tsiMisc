\name{percentTrue}
\alias{percentTrue}
\title{Percentual ratio of TRUE cases.}
\description{`percent` computes the percentual ratio of TRUE cases}
\usage{percentTrue(x, digits = NULL)}
\arguments{
  \item{x}{a logical vector (or a vector that can be coerced to one)}
  \item{digits}{number of decimal places (passed to 'round')}
}

\value{Percentual ratio of TRUE cases of 'x'.}

\author{Tomas Sieger <tomas.sieger@seznam.cz> [aut, cre]}




\examples{
percentTrue(c(TRUE, TRUE, TRUE))
percentTrue(c(TRUE, FALSE, FALSE), digits = 1)
percentTrue(c(FALSE))
percentTrue(NULL)
}
