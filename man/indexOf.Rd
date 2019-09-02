\name{indexOf}
\alias{indexOf}
\title{Indices of member(s) in vector.}
\description{\code{\link{indexOf}} finds indices of value(s) in a given vector.

sealso<< regexpr}
\usage{indexOf(x, v, regexp = TRUE)}
\arguments{
  \item{x}{value(s) to be searched for in vector \code{v}}
  \item{v}{vector}
  \item{regexp}{If \code{TRUE}, \code{x} of character type is
interpreted as a regular expressions.}
}



\author{Tomas Sieger}




\examples{
# numeric examples
indexOf(1,c(2,3,1,4))
indexOf(10,c(2,3,1,4))

# character examples
indexOf(c('a','b'),c('a','c','b','d','ab'))
# using regular expressions (enabled by default):
indexOf(c('a.*','b'),c('a','c','b','d','ab'))
# disabling regular expressions:
indexOf(c('a*','b'),c('a','c','b','d','ab'),regexp=FALSE)
}
