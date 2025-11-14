buildSiteDoc<-structure(
function # build_site build
##description<<
## **OBSOLETED**
## This is a simple delegate wrapper for
## \code{staticdocs::build_site} to generate package doc for an
## R package. In addition, if there are any rgl devices opened by
## \code{\link[staticdocs]{build_site}}, they get closed.
##
## TODO: Remove this rgl-closing hack.
(path = '~/src/Rweb/tsiMisc' ##<< path to an R package source
) {
  if (is.null(path) || !file.exists(path)) {
    stop('a path to an R package needed')
  }
  if (requireNamespace('rgl')) {
    devList<-rgl::rgl.dev.list()
  } else {
    devList<-NULL
  }
  staticdocs::build_site(path)
  if (requireNamespace('rgl')) {
    devList<-setdiff(rgl::rgl.dev.list(),devList)
    for (dev in devList) {
      rgl::rgl.set(dev)
      rgl::rgl.close()
    }
  }
},ex=function() {
  ##dontrun<<
  # buildSiteDoc('~/src/R/myPackage')
  ##end<<
})
