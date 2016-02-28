buildInlineDoc<-structure(
function # inlinedocs build
##<< details
## This is simple a delegate wrapper for 'inlinedocs::package.skeleton.dx' to
## to generate inline docs for an R package.
(path = '~/src/Rweb/tsiMisc' ##<< path to R package source
) {
  if (is.null(path) || !file.exists(path)) {
    stop('a path to an R package needed')
  }
  inlinedocs::package.skeleton.dx(path)
},ex=function() {
  ##dontrun<<
  # buildInlineDoc('~/src/R/myPackage')
  ##end<<
})
