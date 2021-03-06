\name{normality2color}
\alias{normality2color}
\title{Sample normality color code.}
\description{\code{\link{normality2color}} estimates the normality of vector
\code{x} and returns a color code representing the result of a
normality test.
}
\usage{normality2color(x, cols = c("green", 
    "red", "gray"))}
\arguments{
  \item{x}{a numeric vector}
  \item{cols}{a vector of three colors, the
first used to represent normality, the second non-normality, and the
third undetermined normality.}
}



\author{Tomas Sieger}



\seealso{\code{\link{normality2flag}}, \code{\link{dagostinoTest}}}
\examples{
normality2color(rnorm(30))
normality2color(rnorm(3))
normality2color(runif(30))
}
