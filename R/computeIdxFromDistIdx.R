computeIdxFromDistIdx<-structure(
function # Indices of two samples of given index into distance matrix.
##description<<
## \code{\link{computeIdxFromDistIdx}} computes the indices of two
## points whose distance appears at given position in a distance matrix
## (as computed by \code{\link[stats]{dist}}).
##seealso<< \code{\link{computeDistIdx}}
(n, ##<< number of points
i ##<< index into the distance matrix
) {
  i1<-floor(n+1/2-sqrt(n^2-n+1/4-2*(i-1)))
  i2<-i-(i1-1)*(n-i1/2)+i1;
  return(c(i1,i2))
  ### a vector of two indices of points whose distance appears at given
  ### position in a distance matrix.
},ex=function() {
  x <- 1:10
  d <- dist(x)

  # entries holding distances between point 3 and 7
  i<-computeDistIdx(length(x), 3, 7)
  print(i)

  # restore indices of the points:
  ii<-computeIdxFromDistIdx(length(x), i)
  print(ii)

})
