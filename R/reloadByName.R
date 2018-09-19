reloadByName<-structure(
function # Reload R package(s).
##description<<
## This is a simple wrapper for
## \code{\link[devtools]{reload(devtools::inst(pkgName))}}.
(pkgName = 'tsiMisc' ##<< name (or a vector of names) of R package(s) to be reloaded
) {
  
  # attempt to accept also non-quoted package names
  nm<-deparse(substitute(pkgName))
  # a hack - if there is no quoted quote, the argument is not a
  # character string (not we can't simply use `is.character()' as this
  # would have evaluated the argument which is NOT a name of a
  # variable)
  if (regexpr('\"',nm)==-1) {
    # use the deparsed argument as the package
    pkgName<-nm
  }
  
  for (x in pkgName) {
    catnl(paste0('unloading ',x))
    devtools::unload(devtools::inst(x))
  }
  for (x in rev(pkgName)) {
    catnl(paste0('loading ',x))
    require(x,character.only=TRUE)
  }
},ex=function() {
  library(MASS)
  reloadByName('MASS')
  # or without quotes as
  reloadByName(MASS)
})
