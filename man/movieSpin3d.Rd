\name{movieSpin3d}
\alias{movieSpin3d}
\title{Create a movie by rotating a 3D rgl scene.}

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
