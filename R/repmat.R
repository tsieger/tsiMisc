repmat<-structure(
function # Repeat matrix.
##description<<
## Create a big matrix by tiling given matrix m*n times in a
## rectangular grid. 'repmat' resembles the matlab function of the same
## name.
(x, ##<< a matrix
m, ##<< number of repetitions in dimension 1 (row multiply factor)
n ##<< number of repetitions in dimension 2 (column multiply factor)
) {
  if (!is.matrix(x)) x<-as.matrix(x)
  mx<-dim(x)[1]
  nx<-dim(x)[2]
  return(matrix(t(matrix(x,mx,nx*n)),mx*m,nx*n,byrow=T))
  ### A matrix of size '(m*r) x (n*c)', where 'r' ('c') represent
  ### the number of rows (columns) of 'x'.
},ex=function() {
  x<-matrix(1:6,2)
  x
  repmat(x,2,3)
})
