\name{mergeSortedLists}
\alias{mergeSortedLists}
\title{Merge sorted numeric lists of almost increasing values.}
\description{Given a list of numeric vectors, in each of which there come almost
increasing sequences (that can be interrupted by decreasing values),
\code{mergeSortedLists} merges them such that
i) the relative order of any two values from an individual vector is
preserved in the result, and
ii) the resulting vector is sorted in increasing order, if this would
not invalidate the first condition.
In case of ties, the value from former vector takes precedence.
For example: the list \code{list(c(1,3,5,3), c(2,4,1,2))} gives the
result of \code{c(1,2,3,4,1,2,5,3)}.
}
\usage{mergeSortedLists(x, dbg = 0)}
\arguments{
  \item{x}{a list of numeric vectors}
  \item{dbg}{debug level}
}

\value{A list of two numeric vectors:
\code{x}, the resulting vector of merged values, and
\code{idx}, the indices of the selected values.}

\author{Tomas Sieger}




\examples{
mergeSortedLists(list(c(1,3,5,3), c(2,4,1,2)),dbg=1)
mergeSortedLists(list(1:10))
mergeSortedLists(list(1:2,1:2))
mergeSortedLists(list(c(),1,c()))
}
