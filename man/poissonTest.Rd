\name{poissonTest}
\alias{poissonTest}
\title{Test of Poisson distribution.}
\description{A simple test of a hypothesis that a given sample 'x' comes from a
Poisson distribution with unknown parameter. The test is described
in Zvara (2008), chapter 10.5.
}
\usage{poissonTest(x, alpha = 0.05, 
    na.rm = FALSE)}
\arguments{
  \item{x}{a vector of samples}
  \item{alpha}{significance level}
  \item{na.rm}{shall NA's be removed first?}
}

\value{a list of 'q' (the test statistics), 'q1, q2' (the critical
values), and 'p' (the p-value of the test)}
\references{Karel Zv\'{a}ra. _Regrese._ Matfyzpress Praha 2008.
Chapter 10.5}
\author{Tomas Sieger}




\examples{
poissonTest(rpois(20,3))
poissonTest(abs(rnorm(20)))
}
