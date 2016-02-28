cartesian2polar<-structure(
function # Cartesian to polar coordinate conversion.
##<< details
##<<seealso cartesian2spherical, polar2cartesian
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
  return(c(r=r,phi=phi))
  ### A vector of two polar coordinates \eqn{r, phi} of a
  ### point represented as \eqn{x, y} in Cartesian coordinates.
},ex=function() {
  cartesian2polar(c(0, 0))
  cartesian2polar(c(1, 2))
})
