plotd<-structure(
function # Plot with defaults.
##description<<
## \code{\link{plotd}} is just a shortcut to \code{plot}
## passing it some default values (no frame and large points).
(... ##<< parameters to \code{plot}
) {
  plot(...,frame=FALSE,pch=19)
},ex=function() {
  plotd(1:10)
})
