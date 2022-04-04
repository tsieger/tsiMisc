spherical2cartesian<-structure(
function # Spherical to Cartesian coordinate conversion.
##description<<
## \code{\link{spherical2cartesian}} provides spherical to
## Cartesian coordinate conversion.
##
##seealso<< \code{\link{cartesian2spherical}}
(r, ##<< a vector of radii (or a vector of length 3 holding \eqn{r, theta,
## phi}, or a matrix of 3 columns)
theta = NULL, ##<< a vector of inclinations
phi = NULL ##<< a vector of azimuths
) {
  if (!missing(theta) && missing(phi)) stop('phi arg missing')
  if (!is.matrix(r)) {
    if (length(r)==3) {
      if (!missing(theta)) {
        # form a vector of r values
        r<-cbind(r)
      } else {
        # form a row of r,theta,phi values
        r<-rbind(r)
      }
    } else {
      r<-as.matrix(r)
    }
  }
  if (!missing(theta)) {
    if (!is.matrix(theta)) theta<-as.matrix(theta)
  }
  if (!missing(phi)) {
    if (!is.matrix(phi)) phi<-as.matrix(phi)
  }
  if (!missing(theta)) {
    if (!all(nrow(r)==c(nrow(theta),nrow(phi)))) stop('incompatible r, theta, and phi')
  }
  if (missing(theta)) {
    if (ncol(r)!=3) stop('theta missing, but r has not 3 columns to supply theta and phi')
    theta<-r[,2]
    phi<-r[,3]
    r<-r[,1]
  }

  return(cbind(
    x=r*sin(theta)*cos(phi),
    y=r*sin(theta)*sin(phi),
    z=r*cos(theta)))
  ### A matrix with \eqn{x}, \eqn{y}, and \eqn{z} Cartesian coordinates.
},ex=function() {
  spherical2cartesian(1, 0, pi / 2)
  spherical2cartesian(1, pi, pi / 4)
  spherical2cartesian(c(1, pi, pi / 4))
  spherical2cartesian(cbind(1,2,1:10))
})
