\name{logit}
\alias{logit}
\title{The logit function.}
\description{'logit' computes 'log( p / (1-p))', i.e. maps the interval '[0, 1]'
to the interval of '(-inf, inf)'.
}
\usage{logit(p)}
\arguments{
  \item{p}{a number in the range of <0, 1>}
}

\value{logit}

\author{Tomas Sieger <tomas.sieger@seznam.cz> [aut, cre]}



\seealso{'expit'}
\examples{
logit(0)
logit(.5)
logit(1)
}
