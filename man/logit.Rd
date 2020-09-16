\name{logit}
\alias{logit}
\title{The logit function.}
\description{\code{\link{logit}} computes \eqn{log( \code{p} / (1-p))}, i.e. maps the interval \eqn{[0, 1]}
to the interval of \eqn{(-\infty, \infty)}.
}
\usage{logit(p)}
\arguments{
  \item{p}{a number in the range of <0, 1>}
}

\value{logit}

\author{Tomas Sieger}



\seealso{\code{\link{expit}}}
\examples{
logit(0)
logit(.5)
logit(1)
}
