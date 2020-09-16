\name{normality2flag}
\alias{normality2flag}
\title{Sample normality estimate flag.}
\description{\code{\link{normality2flag}} estimates the normality of a
vector vector \code{x} and
returns a discrete flag assessing the normality (normal,
non-normal, don't know).

TODO: allow user-supplied normality test?
}
\usage{normality2flag(x)}
\arguments{
  \item{x}{a numeric vector to be tested for normality}
}

\value{a flag of value 1 if normality is not rejected at the 5\% level,
2 if normality is rejected at the 5\% level, and 3 if normality
can't be assesed (not enough data etc.)}

\author{Tomas Sieger}



\seealso{\code{\link{dagostinoTest}}, \code{\link{normality2color}}}
\examples{
normality2flag(rnorm(30))
normality2flag(rnorm(3))
normality2flag(runif(30))
}
