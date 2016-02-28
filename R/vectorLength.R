vectorLength<-structure(
function # Vector length.
##<< details
(v ##<< a vector
) {
  return(sqrt(sum(v^2)))
  ### vetor length
},ex=function() {
  vectorLength(c(1, 0, 0))
  vectorLength(c(1, 1, 0))
  vectorLength(c(1, 1, 1))
  vectorLength(c(-1, 2, 3))
})
