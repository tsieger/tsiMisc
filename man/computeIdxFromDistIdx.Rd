\name{computeIdxFromDistIdx}
\alias{computeIdxFromDistIdx}
\title{Indices of two samples of given index into distance matrix.}
\description{\code{\link{computeIdxFromDistIdx}} computes the indices of two
points whose distance appears at given position in a distance matrix
(as computed by \code{\link[stats]{dist}}).}
\usage{computeIdxFromDistIdx(n, i)}
\arguments{
  \item{n}{number of points}
  \item{i}{index into the distance matrix}
}

\value{a vector of two indices of points whose distance appears at given
position in a distance matrix.}

\author{Tomas Sieger <tomas.sieger@seznam.cz> [aut, cre]}



\seealso{computeDistIdx}
\examples{
x <- 1:10
d <- dist(x)

# entries holding distances between point 3 and 7
i<-computeDistIdx(length(x), 3, 7)
print(i)

# restore indices of the points:
ii<-computeIdxFromDistIdx(length(x), i)
print(ii)

}
