\name{rglSetMouseCbTrackball}
\alias{rglSetMouseCbTrackball}
\title{Link the current device with others to share mouse control using the 'trackball' mode.}

\usage{rglSetMouseCbTrackball(dev = rgl.cur(), button = 1, affectSubscenes = TRUE)}
\arguments{
  \item{dev}{device(s) to set callback for}
  \item{button}{button to set callback for}
  \item{affectSubscenes}{if TRUE, interaction affects all subscenes
in a device, not only the current one}
}



\author{Tomas Sieger}




\examples{
dev1 <- open3d()
shade3d(cube3d(color = rep(rainbow(6), rep(4, 6))))
dev2 <- open3d()
mfrow3d(1, 2, sharedMouse = TRUE)
shade3d(cube3d(color = rep(rainbow(6), rep(4, 6))))
next3d()
shade3d(cube3d(color = rep(rainbow(6), rep(4, 6))))
rglSetMouseCbTrackball(c(dev1, dev2))
}
