countIntersect<-structure(
function # Cardinality of set intersections.
##description<<
## Given \code{n} sets, \code{\link{countIntersect}} computes the number of elements
## being shared by the individual \code{n*(n-1)/2} pairs of sets.
(..., ##<< several numeric or character vectors to intersect;
## alternatively, a data frame or matrix can be supplied, whose columns
## will be treated as individual sets
logical = FALSE ##<< should
) {
  args<-list(...)
  n<-length(args)
  if (n>0) {
    if (n==1 && (is.data.frame(args[[1]]) || is.matrix(args[[1]]))) {
      singleArg<-TRUE
      getArg<-function(i) args[[1]][,i]
      n<-ncol(args[[1]])
    } else {
      singleArg<-FALSE
      getArg<-function(i) args[[i]]
    }
    m<-matrix(NA,n,n)
    for (i1 in 1:n) {
      for (i2 in 1:n) {
        if (i1==i2) {
          if (logical) {
            m[i1,i2]<-sum(getArg(i1))
          } else {
            m[i1,i2]<-length(getArg(i1))
          }
        } else {
          if (logical) {
            m[i1,i2]<-sum(getArg(i2)&getArg(i1))
          } else {
            m[i1,i2]<-sum(getArg(i2)%in%getArg(i1))
          }
        }
      }
    }
  } else {
    m<-NULL
  }
  if (singleArg) {
    rownames(m)<-colnames(m)<-colnames(args[[1]])
  } else {
    rownames(m)<-colnames(m)<-sapply(as.list(substitute(list(...)))[-1],deparse)
  }
  return(m)
  ### A matrix of size \code{n*n}. If \code{logical==FALSE}, on the diagonal,
  ### there are numbers of elements in each set, and at position \code{i,j}
  ### there is the number of elements in set \code{j} shared with set \code{i}.
  ### If \code{logical==TRUE}, on the diagonal, there are numbers of \code{TRUE}s
  ### in each set, and at position \code{i,j} there is the number of \code{TRUE}s
  ### shared by sets \code{i} and \code{j}.
},ex=function() {
  countIntersect(1:2,1:3)
  countIntersect(c('a','b'),c('a','c'),c('b','c'))
})
