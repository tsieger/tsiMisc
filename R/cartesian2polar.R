cartesian2polar<-structure(
function # Cartesian to polar coordinate conversion.
##description<<
## Cartesian to polar coordinate conversion.
##
##seealso<< \code{\link{cartesian2spherical}}, \code{\link{polar2cartesian}}
(x, ##<< vector of x coordinates (or a vector of length 2 holding
## \eqn{x, y}, or a matrix of two columns holding x and y coordinates)
y = NULL ##<< vector y coordinates
) {
  if (!is.matrix(x)) {
    if (length(x)==2) {
      if (!missing(y)) {
        # form a vector of x values
        x<-cbind(x)
      } else {
        # form a row of x,y values
        x<-rbind(x)
      }
    } else {
      x<-as.matrix(x)
    }
  }
  if (!missing(y)) {
    if (!is.matrix(y)) y<-as.matrix(y)
    if (nrow(x)!=nrow(y)) stop('incompatible x and y')
  } else {
    # missing y
    if (ncol(x)==2) {
      y<-x[,2]
      x<-x[,1]
    } else {
      stop('y arg missing')
    }
  }
  r<-sqrt(x^2+y^2)
  phi<-atan2(y,x)
  return(cbind(r=r,phi=phi))
  ### A matrix of the \eqn{r} and \eqn{phi} polar coordinates in columns.
},ex=function() {
  cartesian2polar(0, 0)
  cartesian2polar(1, 2)
  cartesian2polar(c(1, 2))
  cartesian2polar(cbind(1:10,1:10))
})
