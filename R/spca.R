spca<-structure(
function # Supervised PCA.
##description<<
## \code{\link{spca}} computes supervised principal component analysis as
## described in Barshan et al.
##
##references<< Barshan, E., Ghodsi, A., Azimifar, Z., Jahromi, M. Z.
## _Supervised principal component analysis: Visualization,
## classification and regression on subspaces and submanifolds_.
## Pattern Recognition, Vol. 44, No. 7. (29 July 2011), pp. 1357-1371,
## doi:10.1016/j.patcog.2010.12.015.
(x, ##<< a data matrix (features in columns, samples in rows)
y = diag(1, nrow(x)), ##<< target classification of x (logical, numeric,
## or a factor), or a kernel matrix of the target. If not specified, it
## defaults to identity matrix, in which case SPCA becomes equivalent
## to classical PCA (as the matrix being decomposed equals the
## covariance matrix of 'x'. (Strictly speaking, when centering is in
## use, SPCA becomes the classical PCA. Otherwise, SPCA yields
## components similar to those yielded by PCA over centered data, but
## shifted.)
center = TRUE, ##<< a logical value indicating whether to center the
## data. This is advisable.
scale = FALSE, ##<< a logical value indicating whether to scale the
## data to have unit variance.
retx = FALSE, ##<< a logical value indicating whether to return the
## rotated version of 'x'
debug = FALSE ##<< if TRUE, debugs will be printed. If numeric of value
## greater than 1, verbose debugs will be produced.
) {
  if (!is.matrix(x)) x<-as.matrix(x)

  if (is.logical(center) && center) {
    x<-scale(x,center=TRUE, scale=FALSE)
    .center<-attr(x,'scaled:center')
  } else {
    .center<-FALSE
  }
  if (is.logical(scale) && scale) {
    x<-scale(x,center=FALSE,scale=TRUE)
    .scale<-attr(x,'scaled:scale')
  } else {
    .scale<-FALSE
  }
  n<-nrow(x)
  H<-diag(1,n)-matrix(1/n,n,n)
  if (is.matrix(y)) {
    L<-y
  } else {
    if (!is.factor(y)) y<-as.factor(y)
    L<-matrix(0,n,n)
    for (i in levels(y)) {
      tmp<-y==i
      L[tmp,tmp]<-1
    }
  }
  if (debug) {
    .pn(x)
    .pn(H)
    .pn(L)
  }
  Q<-t(x)%*%H%*%L%*%H%*%x
  if (debug) {
    .pn(Q)
  }
  U<-eigen(Q,symmetric=TRUE)
  if (debug) {
    .pn(U)
  }
  U$Q<-Q
  if (retx) {
    U$x<-x%*%U$vectors
  }
  if (!is.null(colnames(x))) {
    rownames(U$vectors)<-colnames(x)
  }
  colnames(U$vectors)<-paste0('SPC',1:ncol(U$vectors))
  U$center<-.center
  U$scale<-.scale
  return(U)
  ### Eigenvalue decomposition of \code{Q} (see the paper). The value is a
  ### list of \code{values} and \code{vectors} components (see
  ### \code{\link[base]{eigen}},
  ### \code{Q}, the matrix being decomposed, and \code{center} and \code{scale}
  ### holding the centering and scaling used, or \code{FALSE}.
  ### If \code{retx} is \code{TRUE}, the rotated version of \code{x}
  ### is returned in \code{x}.
  ### The number of eigenvalues and eigenvectors correspond to the
  ### dimension of the output space.
},ex=function() {
  spca(iris[,1:4],iris$Species)
})
