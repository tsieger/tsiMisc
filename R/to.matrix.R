to.matrix<-structure(
function # Conversion to a matrix.
##<< details
## 'to.matrix' attempts to convert any data to a row matrix. In
## particular, data frames are converted by 'as.matrix', (numeric)
## vectors are converted to a single row matrix.
(x ##<<
)
{
  if (!is.matrix(x)) {
    if (is.data.frame(x)) {
      x<-as.matrix(x)
    } else {
      x<-matrix(x,nrow=1)
    }
  }
  return(x)
  ### matrix
},ex=function() {
  to.matrix(1:3)
  to.matrix(iris[1:3,1:4])
  to.matrix(matrix(1:4,2))
})
