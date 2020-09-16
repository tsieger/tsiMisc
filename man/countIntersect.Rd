\name{countIntersect}
\alias{countIntersect}
\title{Cardinality of set intersections.}
\description{Given \code{n} sets, \code{\link{countIntersect}} computes the number of elements
being shared by the individual \code{n*(n-1)/2} \code{\link{pairs}} of sets.}
\usage{countIntersect(..., logical = FALSE)}
\arguments{
  \item{\dots}{several numeric or character vectors to intersect;
alternatively, a data frame or matrix can be supplied, whose columns
will be treated as individual sets}
  \item{logical}{should}
}

\value{A matrix of size \code{n*n}. If \code{logical==FALSE}, on the diagonal,
there are numbers of elements in each set, and at position \code{i,j}
there is the number of elements in set \code{j} shared with set \code{i}.
If \code{logical==TRUE}, on the diagonal, there are numbers of \code{TRUE}s
in each set, and at position \code{i,j} there is the number of \code{TRUE}s
shared by sets \code{i} and \code{j}.}

\author{Tomas Sieger}




\examples{
countIntersect(1:2,1:3)
countIntersect(c('a','b'),c('a','c'),c('b','c'))
}
