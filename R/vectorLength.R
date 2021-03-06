vectorLength<-structure(
function # Vector length.
##description<<
## \code{\link{vectorLength}} computes the length of a vector.
(v ##<< a vector
) {
  return(sqrt(sum(v^2)))
  ### vector length
},ex=function() {
  vectorLength(c(1, 0, 0))
  vectorLength(c(1, 1, 0))
  vectorLength(c(1, 1, 1))
  vectorLength(c(-1, 2, 3))
})
