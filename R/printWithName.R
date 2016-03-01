.pn<-printWithName<-structure(
function # Print the name and value of a variable.
##description<<
## 'printWithName' prints both the name and value of a variable. Useful
## for debug purposes.
##
##alias<< .pn
##
##seealso<< 'NCmisc::preview', 'NCmisc::prv'
(x ##<< variable
) {
  cat(paste0(deparse(substitute(x)),'\n'))
  print(x)
},ex=function() {
  a<-1:10
  .pn(a)
})
