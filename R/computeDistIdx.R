computeDistIdx<-structure(
function # Indices into distance matrix.
##description<<
## \code{\link{computeDistIdx}} computes the indices of entries in a
## distance matrix (as computed by \code{\link[stats]{dist}}) that hold
## the distances between a given point and all the other points.
(n, ##<< number of points
i, ##<< index of point to compute indices for
includeMyself = FALSE ##<< if TRUE, the returned vector includes an
## extra \code{NA} element corresponding to the non-existing index into
## the distance matrix holding the distance between point \code{i} and
## point \code{i} itself. This makes the length of the returned vector
## to be equal to \code{n}, such that the index corresponding to the
## distance between point \code{i} and point \code{j} resides at
## position \code{j} in the returned vector.
) {
  if (i>n) stop('i must be less or equal to n')
  if (i==1) {
    idx<-1:(n-1)
    if (includeMyself) idx<-c(NA,idx)
  } else {
    # from 1 ... i-1 to i
    idx<-i-1+cumsum(c(0,rseq(n-2,n-2-(i-3),-1)))
    if (includeMyself) idx<-c(idx,NA)
    # from i to i+1 ... n
    tmp<-1+(2*n-i)*(i-1)/2
    idx<-c(idx,rseq(tmp,tmp+n-i-1,1))
  }
  return(idx)
  ### a vector of indices of entries in the distance matrix related to
  ### the distances from the \code{i}-th point to the other points.
  ### The indices of entries corresponding to the distance between
  ### point \code{j} and point \code{i} are sorted in increasing order
  ### according to \code{j}. If \code{includeMyself} is \code{TRUE},
  ### the returned vector includes an extra \code{NA} element
  ### corresponding to the non-existing index into the distance matrix
  ### holding the distance between point \code{i} and point \code{i}
  ### itself. This makes the length of the returned vector to be equal
  ### to \code{n}, such that the index corresponding to the distance
  ### between point \code{i} and point \code{j} resides at position
  ### \code{j} in the returned vector.
},ex=function() {
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
})
