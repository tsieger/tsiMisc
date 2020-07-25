.pn<-printWithName<-structure(
function # Print the name and value of a variable.
##description<<
## \code{\link{printWithName}} prints both the name and value
## of a variable. Useful for debug purposes.
##
##alias<< \code{\link{.pn}}
##
##seealso<< \code{\link[NCmisc]{preview}}, \code{\link[NCmisc]{prv}}.
(x, ##<< variable
heading = NULL##<< optional heading for the variable print
) {
  if (!is.null(heading)) {
    heading<-paste0(heading,': ')
  }
  cat(paste0(heading,deparse(substitute(x)),'\n'))
  print(x)
},ex=function() {
  a<-1:10
  .pn(a)
})
