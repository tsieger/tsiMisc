\name{rseq}
\alias{rseq}
\title{Robust sequence generation resembling the matlab ':' operator.}
\description{\code{rseq} is similar to \code{\link[base]{seq}} with the only
difference in the case when \code{by} is specified and goes in the
opposite direction to \code{from} to \code{to}. In this case
\code{\link[base]{seq}} raises an error, while \code{rseq} returns
an empty sequence.}
\usage{rseq(from = 1, to = 1, by = NULL, ...)}
\arguments{
  \item{from}{from}
  \item{to}{to}
  \item{by}{increment}
  \item{\dots}{
}
}

\value{Sequence similar to the result of \code{\link[base]{seq}}.}

\author{Tomas Sieger <tomas.sieger@seznam.cz> [aut, cre]}




\examples{
rseq(from = 1, to = 0, by = 1)
}
