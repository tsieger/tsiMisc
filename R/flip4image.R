flip4image<-structure(
function # Flip a matrix to be passed to `image'.
##description<<
## Flip a matrix such that it will appear `as is' when plotted by `image'.
## Note that `image' shows the rows of the matrix on the x axis and the
## columns of the matrix on the y axis (with column 1 at the bottom),
## such that the matrix gets displayed rotated 90 degree clockwise.
## `flip4image' therefore rotates the matrix 90 degree
## coutner-clockwise, such that the rotation made by `image' results in
## the desired form of the matrix.
(x ##<< a matrix
) {
  if (!is.matrix(x)) x<-as.matrix(x)
  # transpose and flip columns
  x<-flip(t(x),2)
  return(x)
  ### The matrix 'x' prepared to be displyed by `image'.
},ex=function() {
  # plot a toy matrix
  m<-rbind(1:10,c(1:3,14:20),20*sin(seq(0,4*pi,length=10)))
  m
  image(flip4image(m),col=gray.colors(20))
  # with labels
  image(x=1:10,y=1:3,z=flip4image(m),col=gray.colors(20),yaxt='n')
  axis(2,at=1:3,labels=rev(1:3))

  # plot a covariance matrix
  m<-cov(matrix(rnorm(100),10,10))
  image(flip4image(m),col=gray.colors(20))
  # plot the Cholsky decomposition of the a covariance matrix (an upper triangular matrix)
  image(flip4image(chol(cov(m))),col=gray.colors(20))
})
