\name{create2dData}
\alias{create2dData}
\title{Create 2D data set interactively.}
\description{'create2dData' allows you to interactively create a 2D data set
by clicking points in 2D.
}
\usage{create2dData(xyRanges = c(0, 1, 0, 1), setupDevice = TRUE, ...)}
\arguments{
  \item{xyRanges}{ranges of 2D space (a numeric vector of length 4
giving the xmin, xma, ymin, and ymax limits)}
  \item{setupDevice}{logical flag specifying whether to setup the graphical device.
Setting to FALSE enables to call `create2dData' several times
getting incrementally more and more points.}
  \item{\dots}{optinal parameters passed to `points'}
}
\details{The user places points in 2D by clicking in a graphical device, and
gets coordinates of the clicked points.

For the usual `X11' device the identification process is
terminated by pressing any mouse button other than the first.  For
the `quartz' device the process is terminated by pressing either
the pop-up menu equivalent (usually second mouse button or
`Ctrl'-click) or the `ESC' key.
}
\value{a list of `x' and `y', each holding the coordinates of selected
points}

\author{Tomas Sieger <tomas.sieger@seznam.cz> [aut, cre]}



\seealso{'identify'}
\examples{
# create first sample
xy<-create2dData()
print(xy)

# create another sample
xy2<-create2dData(setupDevice=FALSE,col='red',pch=19)
print(xy2)
}
