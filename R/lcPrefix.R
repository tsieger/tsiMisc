lcPrefix<-structure(
function # Longest common prefix.
##description<<
## \code{\link{lcPrefix}} returns the longest common prefix of
## a vector of strings.
##
##details<<
## The implementation adapted after 
## \url{https://stackoverflow.com/questions/28273716/r-implementation-for-finding-the-longest-common-starting-substrings-in-a-set-of}
##
##references<< \url{https://stackoverflow.com/questions/28273716/r-implementation-for-finding-the-longest-common-starting-substrings-in-a-set-of}
##
##seealso<< \code{\link{lcPostfix}},\code{\link{rmLcPrefix}}
##
(x, ##<< a vector of character strings
ignore.case = FALSE ##<<
) {
  x <- as.character(x)
  if (length(x)<2) return(x)
  if (ignore.case) x <- toupper(x)
  nc <- nchar(x, type = "char")
  for (i in rseq(1,min(nc),1)) {
    ss <- substr(x, 1, i)
    if (any(ss != ss[1])) {
      return(substr(x[1], 1, i - 1))
    }
  }
  return(substr(x[1], 1, i))
  ### The longest common prefix.
},ex=function() {
  lcPrefix(c('abcd','abef','aqwe'))
})
