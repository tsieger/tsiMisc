txPca<-structure(
function # PCA transform.
##description<<
## 'txPca' transforms data using principal component analysis.
## TODO
##
##seealso<< 'prcomp', 'txSpca', 'plot3dProj'
(x, ##<< a data matrix (features in columns, samples in rows)
k = 3, ##<< number of dimensions of the result, defaults to 3 in order
## to be usable in 'plot3dProj'
... ##<< additional arguments to 'prcomp'
) {
  s<-prcomp(x,retx=TRUE,...)

  if (!is.null(k)) {
    if (k>ncol(s$x)) {
      warning(paste('\'k\' of',k,'greater than the dimensionality of the data',ncol(s$x)))
      k<-ncol(s$x)
    }
  }

  tx<-function(y, center=TRUE) {
    if (center) {
      if (is.numeric(s$center)) {
        y<-t(apply(y,1,function(z)z-s$center))
      }
      if (is.numeric(s$scale)) {
        y<-t(apply(y,1,function(z)z*s$scale))
      }
    }
    y<-y%*%s$rotation
    y<-y[,1:k,drop=FALSE]
    return(y)
  }

  ### Transform function taking two arguments: a data matrix to
  ### transform, and logical determining whether the data are to be
  ### centered, or not.
  return(tx)
},ex=function() {
  tx<-txPca(iris[,1:4])
  plot(tx(iris[1:10,1:4])[,1:2])
})
