lcPostfix<-structure(
function # Longest common postfix.
##description<<
## \code{\link{lcPostfix}} returns the longest common postfix of
## a vector of strings.
##
##details<<
##
##seealso<< \code{\link{lcPrefix}},\code{\link{rmLcPostfix}}
##
(x, ##<< a vector of character strings
ignore.case = FALSE ##<<
) {
  x <- as.character(x)
  if (length(x)<2) return(x)
  if (ignore.case) x <- toupper(x)
  nc <- nchar(x, type = "char")
  for (i in rseq(1,min(nc),1)) {
    ss <- substr(x, nc-i+1, nc)
    if (any(ss != ss[1])) {
      return(substr(x[1], nc-i+2, nc))
    }
  }
  return(substr(x[1], nc-i+1, nc))

  ### A character string vector having the longest common postfix removed.
},ex=function() {
  lcPostfix(c('abab','cdab'))
})
