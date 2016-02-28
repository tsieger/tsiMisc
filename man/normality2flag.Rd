\name{normality2flag}
\alias{normality2flag}
\title{Sample normality estimate flag.}

\usage{normality2flag(x)}
\arguments{
  \item{x}{a numeric vector to be tested for normality}
}

\value{a flag of value 1 if normality is not rejected at the 5\% level,
2 if normality is rejected at the 5\% level, and 3 if normality
can't be assesed (not enough data etc.)}

\author{Tomas Sieger}




\examples{
normality2flag(rnorm(30))
normality2flag(rnorm(3))
normality2flag(runif(30))
}
