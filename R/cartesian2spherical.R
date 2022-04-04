cartesian2spherical<-structure(
function # Cartesian to spherical coordinate conversion.
##description<<
## Cartesian to spherical coordinate conversion.
##
##seealso<< \code{\link{spherical2cartesian}}
(x, ##<< a vector of x coordinates (or a vector of length 3 holding \eqn{x, y, z},
## or a matrix with 3 columns holding \eqn{x, y, z} coordinates)
y = NULL, ##<< a vector of y coordinates
z = NULL ## z a vector of coordinates
) {
  if (!missing(y) && missing(z)) stop('z arg missing')
  if (!is.matrix(x)) {
    if (length(x)==3) {
      if (!missing(y)) {
        # form a vector of x values
        x<-cbind(x)
      } else {
        # form a row of x,y,z values
        x<-rbind(x)
      }
    } else {
      x<-as.matrix(x)
    }
  }
  if (!missing(y)) {
    if (!is.matrix(y)) y<-as.matrix(y)
  }
  if (!missing(z)) {
    if (!is.matrix(z)) z<-as.matrix(z)
  }
  if (!missing(y)) {
    if (!all(nrow(x)==c(nrow(y),nrow(z)))) stop('incompatible x, y, and z')
  }
  if (missing(y)) {
    if (ncol(x)!=3) stop('y missing, but x has not 3 columns to supply y and z')
    y<-x[,2]
    z<-x[,3]
    x<-x[,1]
  }
  r<-sqrt(x^2+y^2+z^2)
  theta<-ifelse(r==0,0,acos(z/sqrt(x^2+y^2+z^2)))
  phi<-atan2(y,x)
  return(cbind(r,theta,phi))
  ### A matrix of three spherical coordinates \eqn{r, theta, phi} of 
  ### points represented as \eqn{x, y, z} in Cartesian coordinates.
},ex=function() {
  cartesian2spherical(0, 0, 0)
  cartesian2spherical(1, 2, 3)
  cartesian2spherical(c(1, 2, 3))
  cartesian2spherical(cbind(0,1,seq(0:10)))
})
