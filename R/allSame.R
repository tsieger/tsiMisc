allSame<-structure(
function # Are all values the same?
##description<<
## Given a vector, are all the values in it the same?
(x ##<< a vector
) {
  require(utils)
  return(all(x==head(x,1)))
  ### TRUE, if all the values are the same (or the vector is empty)
},ex=function() {
  allSame(c(TRUE, TRUE, TRUE))
  allSame(c(TRUE, TRUE, FALSE))
  allSame(c(FALSE, FALSE))
  allSame(NULL)
})
