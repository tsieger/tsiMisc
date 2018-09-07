computeDistIdx<-structure(
function # Indices into distance matrix.
##description<<
## \code{\link{computeDistIdx}} computes the indices of entries in a
## distance matrix (as computed by \code{\link[stats]{dist}}) that hold
## the distances between a given point and one or more other points.
##seealso<<
## computeIdxFromDistIdx
(n, ##<< number of points
i, ##<< index of point to compute distance from
j = setdiff(1:n, i), ##<< index of point(s) to compute distance to
includeMyself = FALSE ##<< if TRUE, and \code{j} equals
## \code{setdiff(1:n, i)}, the returned vector includes an
## extra \code{NA} element corresponding to the non-existing index into
## the distance matrix holding the distance between point \code{i} and
## point \code{i} itself. This makes the length of the returned vector
## to be equal to \code{n}, such that the index corresponding to the
## distance between point \code{i} and point \code{j} resides at
## position \code{j} in the returned vector.
) {
  if (length(i)!=1) stop('`i\' must be of length 1')
  if (i>n) stop('`i\' must be less or equal to `n\'')
  if (any(j>n)) stop('`j\' must be less or equal to `n\'')
  if (i%in%j) stop('`j\' must not contain `i\'')

  if (includeMyself && !(length(j)==n-1 && all(j==setdiff(1:n,i)))) {
    warning('`includeMyself\' set, but `j\' is not `setdiff(1:n, i)\', disabling `includeMyself\'')
    includeMyself<-FALSE
  }
  i1<-j[j<i]
  idx1<-n*(i1-1) - i1*(i1-1)/2 + i-i1
  j2<-j[j>i]
  idx2<-n*(i-1) - i*(i-1)/2 + j2-i
  if (includeMyself) {
    idx<-c(idx1,NA,idx2)
  } else {
    idx<-c(idx1,idx2)
  }

  if (0) { # old implementation
  if (i==1) {
    idx<-j-1
    if (includeMyself) idx<-c(NA,idx)
  } else {
    # from 1 ... i-1 to i
    idx<-i-1+cumsum(c(0,rseq(n-2,n-2-(i-3),-1)))
    if (includeMyself) idx<-c(idx,NA)
    # from i to i+1 ... n
    tmp<-1+(2*n-i)*(i-1)/2
    idx<-c(idx,rseq(tmp,tmp+n-i-1,1))
  }
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
  d[i[4]]

  # entries holding distances between the 3rd point and points 5 and 7
  computeDistIdx(length(x), 3, c(5, 7))

})
