spherical2cartesian<-structure(
function # Spherical to Cartesian coordinate conversion.
##<< details
##<<seealso cartesian2spherical
(r, ##<< radius (or a vector of length 3 holding \eqn{r, theta,
## phi})
theta = NULL, ##<< inclination
phi = NULL ##<< azimuth
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
  ### A vector of three Cartesian coordinates (\eqn{x, y, z}) of a
  ### point represented as (\eqn{r, theta, phi}) in spherical
  ### coordinates.
},ex=function() {
  spherical2cartesian(c(1, 0, pi / 2))
  spherical2cartesian(c(1, pi, pi / 4))
})
