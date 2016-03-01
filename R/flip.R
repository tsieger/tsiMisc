flip<-structure(
function # Flip an array.
##description<<
## Flip an array along some dimension(s), e.g. flip a matrix upside
## down or left to right.
(x, ##<< an array
dim = 1 ##<< dimension(s) along which to flip (in sequence), defaults
## to 1 (i.e. flip rows).
) {
  if (!is.array(x)) x<-as.array(x)
  if (max(dim)>length(dim(x))) {
    stop('\'dim\' argument is ',max(dim),', but \'x\' has only ',length(dim(x)),' dimension(s).')
  }
  for (d in dim) {
    if (dim(x)[d]>0) {
      x<-eval(parse(text=paste(
        'x[',
        paste(rep(',',d-1),collapse=''),
        'dim(x)[d]:1',
        paste(rep(',',length(dim(x))-d),collapse=''),
        ',drop=FALSE]',sep='')))
        #rownames(tmp)<-rev(rownames(x))
    }
  }
  return(x)
  ### The array 'x' having 'dim' dimension flipped.
},ex=function() {
  # flip a matrix
  x<-matrix(1:6,2)
  x
  # flip upside down
  flip(x,1)
  # flip left to right
  flip(x,2)
  # flip both upside down and left to right
  flip(x,1:2)

  # flip a vector
  v<-1:10
  v
  flip(v)

  # flip an array
  a<-array(1:prod(2:4),2:4)
  a
  # flip along dim 1
  flip(a,1)
  # flip along dim 2
  flip(a,2)
  # flip along dim 3
  flip(a,3)
})
