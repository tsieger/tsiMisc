spca<-structure(
function # Supervised PCA.
##<<details
## 'spca' computes supervised principal component analysis as
## described in Barshan et al.
##
##<<references Barshan, E., Ghodsi, A., Azimifar, Z., Jahromi, M. Z.
## _Supervised principal component analysis: Visualization,
## classification and regression on subspaces and submanifolds_.
## Pattern Recognition, Vol. 44, No. 7. (29 July 2011), pp. 1357-1371,
## doi:10.1016/j.patcog.2010.12.015.
(x, ##<< a data matrix (features in columns, samples in rows)
xLabels ##<< classification of x (logical or factor)
) {
  if (!is.matrix(x)) x<-as.matrix(x)
  if (!is.factor(xLabels)) xLabels<-as.factor(xLabels)
  n<-nrow(x)
  H<-diag(1,n)-matrix(1/n,n,n)
  L<-matrix(0,n,n)
  for (i in levels(xLabels)) {
    tmp<-xLabels==i
    L[tmp,tmp]<-1
  }
  Q<-t(x)%*%H%*%L%*%H%*%x
  U<-eigen(Q)
  U$Q<-Q
  ### Eigenvalue decomposition of 'Q' (see the paper). The value is a
  ### list of 'values' and 'vectors' components (see 'base::eigen'),
  ### plus 'Q', the matrix being decomposed. The number of eigen values
  ### and eigenvectors correspond to the dimension of the output space.
  return(U)
},ex=function() {
  spca(iris[,1:4],iris$Species)
})
