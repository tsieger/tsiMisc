\name{rseq}
\alias{rseq}
\title{Robust sequence generation resembling the matlab ':' operator.}
\description{\code{rseq} is similar \code{to} \code{\link[base]{seq}} with the only
difference in the case when \code{by} is specified and goes in the
opposite direction \code{to} \code{from} \code{to} \code{to}. In this case
\code{\link[base]{seq}} raises an error, while \code{rseq} returns
an empty sequence.}
\usage{rseq(from = 1, to = 1, 
    by = NULL, ...)}
\arguments{
  \item{from}{\code{from}}
  \item{to}{\code{to}}
  \item{by}{increment}
  \item{\dots}{
}
}

\value{Sequence similar \code{to} the result of \code{\link[base]{seq}}.}

\author{Tomas Sieger}




\examples{
rseq(from = 1, to = 0, by = 1)
}
