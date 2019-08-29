cartesian2polar<-structure(
function # Cartesian to polar coordinate conversion.
##description<<
## Cartesian to polar coordinate conversion.
##
##seealso<< \code{\link{cartesian2spherical}}, \code{\link{polar2cartesian}}
(x, ##<< x coordinate (or a vector of length 2 holding \eqn{x, y})
y = NULL ##<< y coordinate
) {
  if (length(x)==2  && is.null(y)) {
    y<-x[2]
    x<-x[1]
  }
  names(x)<-names(y)<-NULL
  r<-sqrt(x^2+y^2)
  phi<-atan2(y,x)
  return(list(r=r,phi=phi))
  ### A list of the \eqn{r} and \eqn{phi} polar coordinates.
},ex=function() {
  cartesian2polar(0, 0)
  cartesian2polar(1, 2)
  cartesian2polar(c(1, 2))
})
