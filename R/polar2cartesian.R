polar2cartesian<-structure(
function # Polar to Cartesian coordinate conversion.
##description<<
## \code{\link{polar2cartesian}} provides polar to Cartesian
## coordinate conversion.
##
##seealso<< \code{\link{cartesian2polar}}
(r, ##<< a vector of radii (a or a vector of length 2 holding \eqn{r, phi})
phi = NULL ##<< a vector of angles
) {
  if (length(r)==2 && is.null(phi)) {
    phi<-r[2]
    r<-r[1]
  }
  names(r)<-names(phi)<-NULL

  return(list(
    x=r*cos(phi),
    y=r*sin(phi)))
  ### A list of the \eqn{x} and \eqn{y} Cartesian coordinates.
},ex=function() {
  polar2cartesian(1, 0)
  polar2cartesian(1, pi)
  polar2cartesian(c(1, pi))
})
