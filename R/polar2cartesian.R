polar2cartesian<-structure(
function # Polar to Cartesian coordinate conversion.
##description<<
## \code{\link{polar2cartesian}} provides polar to Cartesian
## coordinate conversion.
##
##seealso<< \code{\link{cartesian2polar}}
(r, ##<< a vector of radii (a or a vector of length 2 holding \eqn{r, phi},
## or a matrix of two columns holding \eqn{r} and \eqn{phi} coordinates)
phi = NULL ##<< a vector of angles
) {
  if (!is.matrix(r)) {
    if (length(r)==2) {
      if (!missing(phi)) {
        # form a vector of r values
        r<-cbind(r)
      } else {
        # form a row of r,phi values
        r<-rbind(r)
      }
    } else {
      r<-as.matrix(r)
    }
  }
  if (!missing(phi)) {
    if (!is.matrix(phi)) phi<-as.matrix(phi)
    if (nrow(r)!=nrow(phi)) stop('incompatible r and phi')
  } else {
    # missing phi
    if (ncol(r)==2) {
      phi<-r[,2]
      r<-r[,1]
    } else {
      stop('phi arg missing')
    }
  }

  return(cbind(
    x=r*cos(phi),
    y=r*sin(phi)))
  ### A matrix of the \eqn{x} and \eqn{y} Cartesian coordinates in columns.
},ex=function() {
  polar2cartesian(1, 0)
  polar2cartesian(1, pi)
  polar2cartesian(c(1, pi))
  polar2cartesian(cbind(1, seq(0,pi,.1)))
})
