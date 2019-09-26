rmLcPrefix<-structure(
function # Remove longest common prefix.
##description<<
## \code{\link{rmLcPrefix}} removes the longest common prefix
## from a vector of strings.
##
##details<<
##
##references<<
##
##seealso<< \code{\link{rmLcPostfix}}, \code{\link{lcPrefix}}
##
(x, ##<< a vector of character strings
ignore.case = FALSE ##<<
) {
  x<-as.character(x)
  if (length(x)<2) return(x)
  p<-lcPrefix(x,ignore.case=ignore.case)
  n<-nchar(p)
  for (i in seq(along=x)) {
    x[i]<-substring(x[i],n+1)
  }
  return(x)
  ### A character string vector having the longest common prefix removed.
},ex=function() {
  rmLcPrefix(c('abcd','abef','aqwe'))
})
