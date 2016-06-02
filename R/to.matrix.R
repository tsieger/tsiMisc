to.matrix<-structure(
function # Conversion to a matrix.
##description<<
## 'to.matrix' attempts to convert any data to a row matrix. In
## particular, data frames are converted by 'as.matrix', (numeric)
## vectors are converted to a single row/column matrix, according to
## the \code{rowMatrix} argument.
(x, ##<< data to convert to a matrix
rowMatrix = TRUE ##<< if \code{TRUE}, numeric vectors are converted to
## a single row matrix, if \code{FALSE}, they get converted to a single
## column matrix
)
{
  if (!is.matrix(x)) {
    if (is.data.frame(x)) {
      x<-as.matrix(x)
    } else {
      if (rowMatrix) {
        xn<-names(x)
        x<-matrix(x,nrow=1)
        if (!is.null(xn)) colnames(x)<-xn
      } else {
        xn<-names(x)
        x<-matrix(x,ncol=1)
        if (!is.null(xn)) rownames(x)<-xn
      }
    }
  }
  return(x)
  ### matrix
},ex=function() {
  to.matrix(1:3)
  to.matrix(1:3,rowMatrix=FALSE)
  to.matrix(iris[1:3,1:4])
  to.matrix(matrix(1:4,2))
})
