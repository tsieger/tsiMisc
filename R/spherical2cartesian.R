spherical2cartesian<-structure(
function # Spherical to Cartesian coordinate conversion.
##description<<
## \code{\link{spherical2cartesian}} provides spherical to
## Cartesian coordinate conversion.
##
##seealso<< \code{\link{cartesian2spherical}}
(r, ##<< a vector of radii (or a vector of length 3 holding \eqn{r, theta,
## phi})
theta = NULL, ##<< a vector of inclinations
phi = NULL ##<< a vector of azimuths
) {
  if (length(r)==3 && is.null(theta) && is.null(phi)) {
    theta<-r[2]
    phi<-r[3]
    r<-r[1]
  }
  names(r)<-names(theta)<-names(phi)<-NULL

  return(c(
    x=r*sin(theta)*cos(phi),
    y=r*sin(theta)*sin(phi),
    z=r*cos(theta)))
  ### A list of \eqn{x}, \eqn{y}, and \eqn{z} Cartesian coordinates.
},ex=function() {
  spherical2cartesian(1, 0, pi / 2)
  spherical2cartesian(1, pi, pi / 4)
  spherical2cartesian(c(1, pi, pi / 4))
})
