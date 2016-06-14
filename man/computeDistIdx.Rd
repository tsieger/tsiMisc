\name{computeDistIdx}
\alias{computeDistIdx}
\title{Indices into distance matrix.}
\description{\code{\link{computeDistIdx}} computes the indices of entries in a
distance matrix (as computed by \code{\link[stats]{dist}}) that hold
the distances between a given point and all the other points.}
\usage{computeDistIdx(n, i)}
\arguments{
  \item{n}{number of points}
  \item{i}{index of point to compute indices for}
}

\value{a vector of indices of entries in the distance matrix related to
the distances from the \code{i}-th point to the other points.
The indices of entries corresponding to the distance between
point \code{j} and point \code{i} are sorted in increasing order
according to \code{j}.}

\author{Tomas Sieger}




\examples{
x<-1:10
d<-dist(x)

# entries holding distances between the 3rd point and all the
# other points
i<-computeDistIdx(length(x),3)
print(i)
}
