computeDistIdx<-structure(
function # Indices into distance matrix.
##description<<
## \code{\link{computeDistIdx}} computes the indices of entries in a
## distance matrix (as computed by \code{\link[stats]{dist}}) that hold
## the distances between a given point and all the other points.
(n, ##<< number of points
i ##<< index of point to compute indices for
) {
  if (i>n) stop('i must be less or equal to n')
  if (i==1) {
    idx<-1:(n-1)
  } else {
    # from 1 ... i-1 to i
    idx<-i-1+cumsum(c(0,seqRob(n-2,n-2-(i-3),-1)))
    # from i to i+1 ... n
    tmp<-1+(2*n-i)*(i-1)/2
    idx<-c(idx,seqRob(tmp,tmp+n-i-1,1))
  }
  return(idx)
  ### a vector of indices of entries in the distance matrix related to
  ### the distances from the \code{i}-th point to the other points.
  ### The indices of entries corresponding to the distance between
  ### point \code{j} and point \code{i} are sorted in increasing order
  ### according to \code{j}.
},ex=function() {
  x<-1:10
  d<-dist(x)

  # entries holding distances between the 3rd point and all the
  # other points
  i<-computeDistIdx(length(x),3)
  print(i)
})
