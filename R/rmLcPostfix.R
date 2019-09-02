rmLcPostfix<-structure(
function # Remove longest common postfix.
##description<<
## \code{\link{rmLcPrefix}} removes the longest common postfix
## from a vector of strings.
##
##details<<
##
##references<<
##
##seealso<< \code{\link{rmLcPrefix}}, \code{\link{lcPostfix}}
##
(x, ##<< a vector of character strings
ignore.case = FALSE ##<<
) {
  x<-as.character(x)
  p<-lcPostfix(x,ignore.case=ignore.case)
  n<-nchar(p)
  for (i in seq(along=x)) {
    x[i]<-substring(x[i],1,nchar(x[i])-n)
  }
  return(x)
  ### A character string vector having the longest common postfix removed.
},ex=function() {
  rmLcPostfix(c('abab','cdab'))
})
