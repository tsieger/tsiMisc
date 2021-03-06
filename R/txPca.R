txPca<-structure(
function # PCA transform.
##description<<
## \code{txPca} transforms data using principal component analysis.
## TODO
##
##seealso<< \code{\link[stat]{prcomp}}, \code{\link{txSpca}},
## \code{\link{plot3dProj}}
(x, ##<< a data matrix (features in columns, samples in rows)
k = 3, ##<< number of dimensions of the result, defaults to 3 in order
## to be usable in \code{\link{plot3dProj}}
... ##<< additional arguments to \code{\link[stats]{prcomp}}
) {
  if (!is.numeric(k) || length(k)!=1) {
    stop('invalid \'k\' argument')
  }

  s<-prcomp(x,retx=FALSE,...)

  # add function taking \code{k}, the number of components, and
  # returning the contribution of individual dimensions to the top
  # \code{k} components
  s$varExplained<-function(k) apply(s$rotation,1,function(x)crossprod(x[1:k]))

  if (!is.null(k)) {
    if (k>ncol(x)) {
      warning(paste('\'k\' of',k,'greater than the dimensionality of the data',ncol(x)))
      k<-ncol(x)
    }
  }

  tx<-function(y, center=TRUE) {
    y<-to.matrix(y)
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
  attr(tx,'params')<-s

  return(tx)
  ### Transform function taking two arguments: a data matrix \code{y}
  ### to transform, and a logical \code{center} determining whether
  ### the data are to be centered, or not. The parameters of the
  ### transform get returned in the \code{params} attribute (see
  ### \code{\link[stats]{prcomp}}).
  ### In addition, there is the \code{varExplained} function added to
  ### the parameters, which takes \code{k}, the number of components,
  ### and returns the contribution of individual dimensions to the top
  ### \code{k} components.
},ex=function() {
  tx<-txPca(iris[,1:4])
  plot(tx(iris[,1:4])[,1:2],pch=19,col=c('red','green','blue')[as.numeric(iris$Species)])

  if (interactive() && require(rgl)) {
    # a 3D example
    x<-iris[,1:4]
    y<-iris$Species
    plot3dProj(x, cls=y, tx=txPca(x))
  }
})
