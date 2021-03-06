\name{rglSetMouseCbTrackball}
\alias{rglSetMouseCbTrackball}
\title{Link the current device with others to share mouse control using the 'trackball' mode.}
\description{'rglSetMouseCbTrackball' sets mouse "trackball" callback for given
\code{button} on selected device(s), such that interaction made using the
given \code{button} affects all the devices (and all their subscenes, by
default).

The code is mostly based on the 'mouseCallbacks' demo from the
'rgl' package.}
\usage{rglSetMouseCbTrackball(dev = rgl.cur(), 
    button = 1, affectSubscenes = TRUE)}
\arguments{
  \item{dev}{device(s) to set callback for}
  \item{button}{\code{button} to set callback for}
  \item{affectSubscenes}{if TRUE, interaction affects all subscenes
in a device, not only the current one}
}



\author{Tomas Sieger}




\examples{
if (interactive() && require(rgl)) {
  dev1 <- open3d()
  shade3d(cube3d(color = rep(rainbow(6), rep(4, 6))))
  dev2 <- open3d()
  mfrow3d(1, 2, sharedMouse = TRUE)
  shade3d(cube3d(color = rep(rainbow(6), rep(4, 6))))
  next3d()
  shade3d(cube3d(color = rep(rainbow(6), rep(4, 6))))
  rglSetMouseCbTrackball(c(dev1, dev2))
}
}
