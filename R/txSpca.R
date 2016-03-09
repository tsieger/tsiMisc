txSpca<-structure(
function # Supervised PCA transform.
##description<<
## 'txSpca' transforms data using supervised principal component
## analysis.
## TODO
##
##seealso<< 'spca', 'txPca', 'plot3dProj'
(x, ##<< a data matrix (features in columns, samples in rows)
y = diag(1, nrow(x)), ##<< target classification of x (logical, numeric,
## or a factor), or a kernel matrix of the target. If not specified, it
## defaults to identity matrix, in which case SPCA becomes equivalent
## to classical PCA (as the matrix being decomposed equals the
## covariance matrix of 'x'. (Strictly speaking, when centering is in
## use, SPCA becomes the classical PCA. Otherwise, SPCA yields
## components similar to those yielded by PCA over centered data, but
## shifted.)
k = 3, ##<< number of dimensions of the result, defaults to 3 in order
## to be usable in 'plot3dProj'
... ##<< additional arguments to 'spca'
) {
  s<-spca(x,y,retx=TRUE,...)

  if (!is.null(k)) {
    if (k>ncol(s$x)) {
      warning(paste('\'k\' of',k,'greater than the dimensionality of the data',ncol(s$x)))
      k<-ncol(s$x)
    }
  }

  tx<-function(y, center=TRUE) {
    y<-to.matrix(y)
    rn<-rownames(y)
    if (center) {
      if (is.numeric(s$center)) {
        y<-t(apply(y,1,function(z)z-s$center))
      }
      if (is.numeric(s$scale)) {
        y<-t(apply(y,1,function(z)z*s$scale))
      }
    }
    y<-y%*%s$vectors
    rownames(y)<-rn
    colnames(y)<-paste0('SPC',1:ncol(y))
    y<-y[,1:k,drop=FALSE]
    return(y)
  }

  ### Transform function taking two arguments: a data matrix to
  ### transform, and logical determining whether the data are to be
  ### centered, or not.
  return(tx)
},ex=function() {
  tx<-txSpca(iris[,1:4],iris$Species)
  plot(tx(iris[1:10,1:4])[,1:2])

  # comparison of PCA vs. SPCA
  # TODO
  opar<-par(mfrow=c(1,2))
  plot(txSpca(iris[,1:4],iris$Species)(iris[,1:4])[,1:2],col=c('red','green','blue')[as.numeric(iris$Species)])
  plot(txPca(iris[,1:4])(iris[,1:4])[,1:2],col=c('red','green','blue')[as.numeric(iris$Species)])
  par(opar)
})