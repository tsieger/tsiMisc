\name{plot3dProj}
\alias{plot3dProj}
\title{Visualize multidimensional data in 3D and/or multiple 2D projections.}
\description{TODO}
\usage{plot3dProj(x, col = NULL, cls = NULL, palette = c("black", "red", 
    "green", "blue"), size = 3, alpha = 1, scale = TRUE, tx = function(y, 
    center = TRUE) y - center * matrix(colMeans(x), nrow = nrow(y), 
    ncol = ncol(x), byrow = TRUE), type = "3aw,sw", col.axes = "gray", 
    axesExpansion = 1.1, annotate = FALSE, debug = FALSE)}
\arguments{
  \item{x}{a data frame or matrix to visualize. Instances in rows,
features in columns. It must have at least 3 columns (dimensions).}
  \item{col}{the
color of individual instances, recycled if necessary}
  \item{cls}{class membership of individual instances. This serves
the only purpose of alternative color specification: if
'col' is NULL ...}
  \item{palette}{color palette to be
used for individual classes specified in 'cls', see the 'col'
argument}
  \item{size}{size of points}
  \item{alpha}{alpha of points}
  \item{scale}{if TRUE, data get scaled to the range of '[-1, 1]'
in all dimensions}
  \item{tx}{transform
function used to transform data from the k-dimensional space of 'x'
into 3D space to visualize}
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
