\name{countIntersect}
\alias{countIntersect}
\title{Cardinality of set intersections.}

\usage{countIntersect(..., logical = FALSE)}
\arguments{
  \item{\dots}{several numeric or character vectors to intersect;
alternatively, a data frame or matrix can be supplied, whose columns
will be treated as individual sets}
  \item{logical}{should}
}

\value{A matrix of size 'n*n'. If 'logical'==FALSE, on the diagonal,
there are numbers of elements in each set, and at position 'i,j'
there is the number of elements in set 'j' shared with set 'i'.
If 'logical'==TRUE, on the diagonal, there are numbers of TRUEs
in each set, and at position 'i,j' there is the number of TRUEs
shared by sets 'i' and 'j'.}

\author{Tomas Sieger}




\examples{
countIntersect(1:2,1:3)
countIntersect(c('a','b'),c('a','c'),c('b','c'))
}
