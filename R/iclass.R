iclass<-structure(
function # Determine implicit class.
##description<<
## \code{\link{iclass}} determines the implicit class of a base type.
## The code is taken from the great "Advanced R" by Hadley Wickham.
##
##references<<
## Wickham, Hadley. _Advanced R_.
## Available online at \url{http://adv-r.had.co.nz/OO-essentials.html}
(x ##<< base type
) {
  if (is.object(x)) {
    stop(paste('not a primitive type, but an object of class:',paste(class(x),collapse=',')), call. = FALSE)
  }

  rv<-c(
    if (is.matrix(x)) "matrix",
    if (is.array(x) && !is.matrix(x)) "array",
    if (is.double(x)) "double",
    if (is.integer(x)) "integer",
    mode(x)
  )
  return(rv)
  ### A vector describing the implicit class of 'x'.
},ex=function() {
  iclass(matrix(1:5))
  #> [1] "matrix"  "integer" "numeric"

  iclass(array(1.5))
  #> [1] "array"   "double"  "numeric"
})
