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
point \code{j < i} and \code{i} get also returned in the
\code{i1} attribute of the return value, and the indices of
entries corresponding to the distances between point \code{j > i}
and \code{i} get returned in the \code{i2} attribute of the
return value. These attributes can be useful to construct a
vector of length \code{n} corresponding to each of the individual
observations distance matrix is computed from, by concatenating
some measure based on \code{i1}, some value relevant to the
point \code{i} itself, and some measure based on \code{i2}.}

\author{Tomas Sieger}




\examples{
x<-1:10
d<-dist(x)

# entries holding distances between the 3rd point and all the
# other points
i<-computeDistIdx(length(x),3)
print(i)

# entries holding distances from 1st and 2nd points to the 3rd point:
print(attr(i,'i1'))
# entries holding distances from the 3rd point to the 4th, ..., 10th
# points:
print(attr(i,'i2'))

}
