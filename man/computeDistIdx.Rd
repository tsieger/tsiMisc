\name{computeDistIdx}
\alias{computeDistIdx}
\title{Indices into distance matrix.}
\description{\code{\link{computeDistIdx}} computes the indices of entries in a
distance matrix (as computed by \code{\link[stats]{dist}}) that hold
the distances between a given point and all the other points.}
\usage{computeDistIdx(n, i, includeMyself = FALSE)}
\arguments{
  \item{n}{number of points}
  \item{i}{index of point to compute indices for}
  \item{includeMyself}{if TRUE, the returned vector includes an
extra \code{NA} element corresponding to the non-existing index into
the distance matrix holding the distance between point \code{i} and
point \code{i} itself. This makes the length of the returned vector
to be equal to \code{n}, such that the index corresponding to the
distance between point \code{i} and point \code{j} resides at
position \code{j} in the returned vector.}
}

\value{a vector of indices of entries in the distance matrix related to
the distances from the \code{i}-th point to the other points.
The indices of entries corresponding to the distance between
point \code{j} and point \code{i} are sorted in increasing order
according to \code{j}. If \code{includeMyself} is \code{TRUE},
the returned vector includes an extra \code{NA} element
corresponding to the non-existing index into the distance matrix
holding the distance between point \code{i} and point \code{i}
itself. This makes the length of the returned vector to be equal
to \code{n}, such that the index corresponding to the distance
between point \code{i} and point \code{j} resides at position
\code{j} in the returned vector.}

\author{Tomas Sieger}




\examples{
x <- 1:10
d <- dist(x)

# entries holding distances between the 3rd point and all the
# other points
computeDistIdx(length(x), 3)

# include an extra \code{NA} at the 3rd position to make the vector easily indexable
i <- computeDistIdx(length(x), 3, includeMyself = TRUE)
i
# the distance between point 3 and 4:
i[4]
}
