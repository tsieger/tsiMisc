normality2color<-structure(
function # Sample normality color code.
##description<<
## \code{\link{normality2color}} estimates the normality of vector
## \code{x} and returns a color code representing the result of a
## normality test.
##
##seealso<< \code{\link{normality2flag}}, \code{\link{dagostinoTest}}
(x, ##<< a numeric vector
cols = c('green', 'red', 'gray') ##<< a vector of three colors, the
## first used to represent normality, the second non-normality, and the
## third undetermined normality.
) {
  return(cols[normality2flag(x)])
  ## a color code representing sample normality
},ex=function() {
  normality2color(rnorm(30))
  normality2color(rnorm(3))
  normality2color(runif(30))
})
