plotShadow<-structure(
function # Plot a shadow matrix.
##details<<
## 'plotShadow' explores missing values in a matrix by plotting the
## shadow matrix of the data, in which missing values are coded as
## black rectangles, and non-missing values are given in light gray.
(x, ##<< a matrix
col = gray.colors(2), ##<< color palette
transpose = FALSE ##<< transpose the matrix?
) {
  plotMatrix(!is.na(x),col=col,transpose=transpose)
},ex=function() {
  x<-matrix(1:36,6)
  rownames(x)<-paste('row',1:6)
  colnames(x)<-paste('column',1:6)
  x[2,3:5]<-NA
  x[6,6]<-NA
  x
  plotShadow(x)
})
