normalizeVector<-structure(
function # Normalize vector to unit length.
##description<<
## \code{normalizeVector} normalizes a vector to unit length.
(v ##<< a vector
) {
  return(v/vectorLength(v))
  ### normalized vector
},ex=function() {
  normalizeVector(c(1, 0, 0))
  normalizeVector(c(1, 1, 0))
  normalizeVector(c(1, 1, 1))
  normalizeVector(c(-1, 2, 3))
  vectorLength(normalizeVector(c(-1, 2, 3)))
})
