\name{plot3dProj}
\alias{plot3dProj}
\title{Visualize multidimensional data in 3D and/or multiple 2D projections.}
\description{TODO}
\usage{plot3dProj(x, col = NULL, cls = NULL, size = 3, alpha = 1, palette = c("black", 
    "red", "green", "blue"), main = NULL, scale = TRUE, tx = function(y, 
    center = TRUE) y - center * matrix(colMeans(x), nrow = nrow(y), 
    ncol = ncol(x), byrow = TRUE), k = ncol(x), dimToShow = NULL, 
    type = "3awm,sw", col.axes = "gray", axesExpansion = 1.1, 
    annotate = FALSE, debug = FALSE)}
\arguments{
  \item{x}{a data frame or matrix to visualize. Instances in rows,
features in columns. It must have at least 3 columns (dimensions).}
  \item{col}{the
color of individual instances, recycled if necessary}
  \item{cls}{class membership of individual instances. This serves
the only purpose of alternative color specification: if
'col' is NULL ...}
  \item{size}{size of points}
  \item{alpha}{alpha of points}
  \item{palette}{color palette to be
used for individual classes specified in \code{cls}, see the
\code{col} argument}
  \item{main}{title}
  \item{scale}{if TRUE, data get scaled to the range of '[-1, 1]'
in all dimensions}
  \item{tx}{Transform
function used to transform data from the k-dimensional space of 'x'
into 3D space to visualize. The function must accept two arguments:
a data matrix to transform, and a logical called \code{center}
specifying whether to center the data prior the transform. The
function must return a 3-column transformed version of the input
data matrix.}
  \item{k}{if \code{tx} is one of \code{\link{txPca}} or
\code{\link{txSpa}}, \code{k} can specify the number of dimensions
to plot. The default is the number of dimensions of \code{x}, but
smaller values are recommended for high-dimensional data in order to
speed up plotting.}
  \item{dimToShow}{if \code{tx} is user-supplied, \code{dimToShow}
can specify which dimensions of \code{x} to plot. \code{dimToShow}
can be a numeric vector consisting of dimensions to show, or a
character vector of the names of dimensions to show.}
  \item{type}{
}
  \item{col.axes}{color of axes in the 3D plot}
  \item{axesExpansion}{
}
  \item{annotate}{
}
  \item{debug}{if TRUE, debugs will be printed. If numeric of value
greater than 1, verbose debugs will be produced.}
}



\author{Tomas Sieger}




\examples{
  if (interactive() && require(rgl)) {
    plot3dProj(iris[,1:3], cls=iris$Species)
  }

# TODO: unify/propagate scaling done in plot3dProj here ?!
#  p <- prcomp(iris[, 1:4], center = TRUE, scale = TRUE)
#  tx <- function (y,center=TRUE) {
#    if(center) y<-y-matrix(colMeans(iris[, 1:4]),nrow=nrow(y),ncol=4,byrow=TRUE)
#    y<-t(t(p$rotation[,1:3])\%*\%t(y))
#    return (y)
#  }
#  plot3dProj(iris[,1:4], tx=tx, cls=iris$Species)

}
