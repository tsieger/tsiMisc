buildInlineDoc<-structure(
function # inlinedocs build
##description<<
## This is simple a delegate wrapper for
## \code{\link[inlinedocs]{package.skeleton.dx}} to
## to generate inline docs for an R package.
(path = '~/src/Rweb/tsiMisc' ##<< path to an R package source
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
