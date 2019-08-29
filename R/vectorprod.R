vectorprod<-structure(
function # 3D vector product.
##description<<
## \code{\link{vectorprod}} computes 3D vector product.
(a, ##<< first vector
b ##<< second vector
) {
  return(cbind(
    a[2]*b[3]-a[3]*b[2],
    a[3]*b[1]-a[1]*b[3],
    a[1]*b[2]-a[2]*b[1]))
  ### Vector product of 'a' and 'b'.
},ex=function() {
  vectorprod(c(1,0,0),c(0,1,0))
  crossprod(c(1,0,0),c(0,1,0))

  vectorprod(c(1,0,0),c(1,0,0))
  crossprod(c(1,0,0),c(1,0,0))
})
