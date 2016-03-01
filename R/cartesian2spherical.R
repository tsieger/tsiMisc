cartesian2spherical<-structure(
function # Cartesian to spherical coordinate conversion.
##description<<
## Cartesian to spherical coordinate conversion.
##
##seealso<< 'spherical2cartesian'
(x, ##<< x coordinate (or a vector of length 3 holding \eqn{x, y, z})
y = NULL, ##<< y coordinate
z = NULL ## z coordinate
) {
  if (length(x)==3  && is.null(y) && is.null(z)) {
    y<-x[2]
    z<-x[3]
    x<-x[1]
  }
  names(x)<-names(y)<-names(z)<-NULL
  r<-sqrt(x^2+y^2+z^2)
  theta<-ifelse(r==0,0,acos(z/sqrt(x^2+y^2+z^2)))
  phi<-atan2(y,x)
  return(c(r=r,theta=theta,phi=phi))
  ### A vector of three spherical coordinates \eqn{r, theta, phi} of a
  ### point represented as \eqn{x, y, z} in Cartesian coordinates.
},ex=function() {
  cartesian2spherical(c(0, 0, 0))
  cartesian2spherical(c(1, 2, 3))
  cartesian2spherical(1, 2, 3)
})
