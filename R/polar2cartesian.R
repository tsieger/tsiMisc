polar2cartesian<-structure(
function # Polar to Cartesian coordinate conversion.
##<< details
##<<seealso cartesian2polar
(r, ##<< radius (or a vector of length 2 holding \eqn{r, phi})
phi = NULL ##<< angle
) {
  if (length(r)==2 && is.null(phi)) {
    phi<-r[2]
    r<-r[1]
  }
  names(r)<-names(phi)<-NULL

  return(c(
    x=r*cos(phi),
    y=r*sin(phi)))
  ### A vector of two Cartesian coordinates (\eqn{x, y}) of a
  ### point represented as (\eqn{r, phi}) in polar
  ### coordinates.
},ex=function() {
  polar2cartesian(c(1, 0))
  polar2cartesian(c(1, pi))
})
