\name{expit}
\alias{expit}
\title{The expit function (the inverse to logit).}
\description{\code{\link{expit}} computes \eqn{exp(x) / (1 + exp(x))}, i.e. the inverse to
\code{\link{logit}}, i.e. it maps the interval \eqn{(-\infty, \infty)}
to the interval of \eqn{[0, 1]}.
}
\usage{expit(x)}
\arguments{
  \item{x}{a number}
}

\value{expit of x}

\author{Tomas Sieger}



\seealso{'logit'}
\examples{
expit(-1e10)
expit(0)
expit(1e10)
}
