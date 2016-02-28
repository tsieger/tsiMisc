indexOf<-structure(
function # Indices of member(s) in vector.
##<<details
## 'indexOf' finds indices of value(s) in a given vector.
##<<sealso regexpr
(x, ##<< value(s) to be searched for in vector 'v'
v, ##<< vector
regexp = TRUE ##<< If TRUE, 'x' of character type is interpreted as
## regular expressions.
) {
  if (is.character(x) && regexp) {
    rv<-c(unlist(sapply(x,function(cn)which(regexpr(paste('^',cn,'$',sep=''),v)!=-1))))
  } else {
    rv<-c(unlist(sapply(x,function(cn)which(cn==v))))
  }
  names(rv)<-NULL
  return(rv)
},ex=function() {
  # numeric examples
  indexOf(1,c(2,3,1,4))
  indexOf(10,c(2,3,1,4))

  # character examples
  indexOf(c('a','b'),c('a','c','b','d','ab'))
  # using regular expressions (enabled by default):
  indexOf(c('a.*','b'),c('a','c','b','d','ab'))
  # disabling regular expressions:
  indexOf(c('a*','b'),c('a','c','b','d','ab'),regexp=FALSE)
})
