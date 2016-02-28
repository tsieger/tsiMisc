\name{poissonTest}
\alias{poissonTest}
\title{Test of Poisson distribution.}

\usage{poissonTest(x, alpha = 0.05, na.rm = FALSE)}
\arguments{
  \item{x}{a vector of samples}
  \item{alpha}{significance level}
  \item{na.rm}{shall NA's be removed first?}
}

\value{a list of 'q' (the test statistics), 'q1, q2' (the critical
values), and 'p' (the p-value of the test)}

\author{Tomas Sieger}




\examples{
poissonTest(rpois(20,3))
poissonTest(abs(rnorm(20)))
}
