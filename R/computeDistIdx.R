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
    i1<-c()
    i2<-c()
  } else {
    i1<-i-1+cumsum(c(0,seq(n-2,n-2-(i-3),-1)))
    tmp<-1+(2*n-i)*(i-1)/2
    i2<-tmp:(tmp+n-i-1)
  }
  i<-c(i1,i2)
  attr(i,'i1')<-i1
  attr(i,'i2')<-i2
  return(i)
  ### a vector of indices of entries in the distance matrix related to
  ### the distances from the \code{i}-th point to the other points.
  ### The indices of entries corresponding to the distance between
  ### point \code{j < i} and \code{i} get also returned in the
  ### \code{i1} attribute of the return value, and the indices of
  ### entries corresponding to the distances between point \code{j > i}
  ### and \code{i} get returned in the \code{i2} attribute of the
  ### return value. These attributes can be useful to construct a
  ### vector of length \code{n} corresponding to each of the individual
  ### observations distance matrix is computed from, by concatenating
  ### some measure based on \code{i1}, some value relevant to the
  ### point \code{i} itself, and some measure based on \code{i2}.
},ex=function() {
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

})
