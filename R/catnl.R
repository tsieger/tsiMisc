catnl<-structure(
function # Cat with trailing newline.
##description<<
## \code{cat} with trailing newline.
(... ##<< arguments to cat
) {
  cat(...,'\n',sep='')
  ### None.
},ex=function() {
  catnl('Hello, world!')
})
