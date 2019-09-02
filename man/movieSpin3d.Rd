\name{movieSpin3d}
\alias{movieSpin3d}
\title{Create a movie by rotating a 3D rgl scene.}
\description{\code{\link{movieSpin3d}} is basically just a wrapper for
\code{movie3d(spin3d(axis=axis, rpm = rpm), fps = fps, ...)}
that sets the duration to the value of '60/rpm-1/fps', which
results in a sequence of frames that form a continuous animation
when repeated. This is in contrast to setting the duration to the
value of 60/rpm, which results in rendering the last frame
identical to the first one, thus making the animation non-smooth.

sealso<< \code{rgl::movie3d}, \code{\link[rgl]{play3d}}}
\usage{movieSpin3d(axis, rpm, fps = 5, ...)}
\arguments{
  \item{axis}{axis to rotate around}
  \item{rpm}{the speed of rotation (rotations per minute)}
  \item{fps}{frame rate (frames per second)}
  \item{\dots}{additional arguments passed to movie3d}
}



\author{Tomas Sieger}




\examples{
##dontrun:
#movieSpin3d(axis=c(1,1,1),rpm=5,fps=4)
}
