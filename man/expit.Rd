\name{expit}
\alias{expit}
\title{The expit function (the inverse to logit).}
\description{'expit' computes 'exp(x) / (1 + exp(x))', i.e. the inverse to
'logit', i.e. it maps the interval '(-inf, inf)' to the interval
of '[0, 1]'.
}
\usage{expit(x)}
\arguments{
  \item{x}{a number}
}

\value{expit of x}

\author{Tomas Sieger <tomas.sieger@seznam.cz> [aut, cre]}



\seealso{'logit'}
\examples{
expit(-1e10)
expit(0)
expit(1e10)
}
