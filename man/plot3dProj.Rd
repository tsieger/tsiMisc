\name{plot3dProj}
\alias{plot3dProj}
\title{Visualize multidimensional data in 3D and/or multiple 2D projections.}
\description{TODO}
\usage{plot3dProj(x, col = NULL, cls = NULL, size = 3, alpha = 1, main = NULL, 
    x2 = NULL, col2 = NULL, cls2 = NULL, size2 = 3, alpha2 = 1, 
    main2 = NULL, x3 = NULL, col3 = NULL, cls3 = NULL, size3 = 3, 
    alpha3 = 1, main3 = NULL, palette = c("black", "red", "green", 
        "blue"), scale = TRUE, tx = function(y) y, dimToShow = NULL, 
    k = NULL, type = "sawm,fw", widths = 1, heights = 1, devices = rgl.cur(), 
    col.axes = "gray", axesExpansion = 1.1, annotateWireFrame = FALSE, 
    ellipses = list(list(center = rep(0, ncol(x)), x = diag(rep(1, 
        ncol(x))), col = "gray", alpha = 0.2)), boxes = list(list(center = rep(0, 
        ncol(x)), scale = rep(0, ncol(x)), col = "gray", alpha = 0.2)), 
    planes = list(list(a = 1, b = 0, c = 0, d = 0, col = "gray", 
        alpha = 0.2)), texts = list(list(x = 0, y = 0, z = 0, 
        text = "text", col = "gray", alpha = 0.2)), debug = FALSE)}
\arguments{
  \item{x}{a data frame or matrix to visualize. Instances in rows,
features in columns. It must have at least 3 columns (dimensions).}
  \item{col}{the color of individual instances
color of individual instances, recycled if necessary}
  \item{cls}{class membership of individual instances. This serves
the only purpose of alternative color specification: if
'col' is NULL ...}
  \item{size}{size of points}
  \item{alpha}{alpha of points}
  \item{main}{title of \code{x}}
  \item{x2}{another data frame or matrix to visualize.}
  \item{col2}{the color of data in \code{x2}}
  \item{cls2}{class membership of data in \code{x2}}
  \item{size2}{size of points in \code{x2}}
  \item{alpha2}{alpha of points in \code{x2}}
  \item{main2}{title of \code{x2}}
  \item{x3}{another data frame or matrix to visualize.}
  \item{col3}{the color of data in \code{x3}}
  \item{cls3}{class membership of data in \code{x3}}
  \item{size3}{size of points in \code{x3}}
  \item{alpha3}{alpha of points in \code{x3}}
  \item{main3}{title of \code{x3}}
  \item{palette}{color palette to be
used for individual classes specified in \code{cls}, see the
\code{col} argument}
  \item{scale}{if TRUE, data get scaled to the range of '[-1, 1]'
in all dimensions}
  \item{tx}{Transform function used to transform data from
the k-dimensional space of \code{x} into 3D space to visualize.
The function takes a data matrix to transform, and is expected to
return a 3-column transformed version of the input data matrix.}
  \item{dimToShow}{a numeric or character vector of dimensions
of \code{x} to plot. If numeric, it indexes the dimensions to show.
If character, it lists  the names of dimensions to show.
The default it to plot all dimensions. \code{dimToShow} takes
precedence over \code{k}, another way to specify the dimensions to
show.}
  \item{k}{if \code{tx} is one of \code{\link{txPca}} or
\code{\link{txSpa}}, \code{k} can specify the number of (most
"significant") dimensions to plot. The default is the number of
dimensions of \code{x}, but smaller values are recommended for
high-dimensional data in order to speed up plotting. Internally, the
\code{k} dimensions selected to be shown are those which contribute
the most to the top \code{k} components (principal or supervised
principal components) of the data. To determine this contribution,
the code calls the \code{varExplained} stored as a name member of a
list attached to the \code{tx} argument as the \code{params}
attribute. This function takes \code{k}, the number of top
components, and returns the contribution of individual dimensions to
these components.}
  \item{type}{a character string defining the type of plots to
produce. There general syntax is \code{<row>;<row>;...;<row>},
where each \code{<row>} stands for \code{<plot>,<plot>,...,<plot>},
and each \code{plot} is a concatenation of one or more plot types to
be combined in the specific plot. The plot types are:
\itemize{
\item \code{a} - \strong{a}xes of the feature space
\item \code{b} - \strong{b}ox(es) as defined by the \code{boxes}
argument
\item \code{d} - \strong{d}ecoration of a 3D plot (axes and
a bounding box, see \code{\link[rgl]{decorated3d}})
\item \code{e} - \strong{e}ellipse(s) as defined by the
\code{ellipses} argument
\item \code{f} - 2D scatter plots on the \strong{f}aces of a
parallelepipedon
\item \code{m} - \strong{m}ain (title) of data
\item \code{p} - \strong{p}lane(s) as defined by the \code{planes}
argument
\item \code{s} - 3D \strong{s}catter plot
\item \code{t} - \strong{t}ext(s) as defined by the \code{texts}
argument
\item \code{w} - \strong{w}ire frame defining the feature space
(the wireframe collides with the edges of the 
parallelepipedon plotted by \code{f})
}
Each of \code{t}, \code{s}, and \code{m} can be followed by a number
referring to a specific data (i.e. \code{x}, \code{x2}, or
\code{x3}). By default, when no number follows the plot type, the
\code{x} is assumed.
For example: \code{tawm,sw} defines two subscenes to be plotted in
one row next to each other. The first plot type \code{tawm} defines
a \strong{t}hree dimensional scatter plot annotated with
\strong{a}xes and enriched with a \strong{w}ire frame and the
\strong{m}ain (title).
The second plot type \code{sw} defines a parallelepipedon having
\strong{s}catter plots on its faces, and enriched with a
\strong{w}ire frame.}
  \item{widths}{relative widths of columns in a multi-subscene scene,
see \code{\link[rgl]{layout3d}}}
  \item{heights}{relative heights of rows in a multi-subscene scene,
see \code{\link[rgl]{layout3d}}}
  \item{devices}{a list of devices to plot at, defaulting to
the current active device, if any. If \code{devices} is NULL, empty,
contains invalid devices, or does not hold enough devices to plot
all the scenes requested by the \code{type} argument, new scene(s)
will be created for such scenes.}
  \item{col.axes}{color of axes in the 3D plot}
  \item{axesExpansion}{
}
  \item{annotateWireFrame}{if \code{TRUE}, the wire frame produced
by \code{type='w'} will be annotates by a black point at
\code{(-1,-1,...,-1)} and numbers 1...\code{k} at the extremes of
the individual \code{k} dimensions.}
  \item{ellipses}{a
list or a list of lists defining ellipses to be plotted by the
\code{'e'} type. TODO}
  \item{boxes}{a
list or a list of lists defining boxes to be plotted by the
\code{'b'} type. TODO}
  \item{planes}{a
list or a list of lists defining planes to be plotted by the
\code{'p'} type. TODO}
  \item{texts}{a
list or a list of lists defining texts to be plotted by the
\code{'t'} type. TODO}
  \item{debug}{if TRUE, debugs will be printed. If numeric of value
greater than 1, verbose debugs will be produced.}
}

\value{a list of rgl device IDs holding the scenes plotted}

\author{Tomas Sieger}




\examples{
  if (interactive() && require(rgl)) {
    # Plot a 3D scatter plot and 2D scatter plots as
    # faces of a parallelepipedon:
    plot3dProj(iris[,1:3], cls=iris$Species)

    # Plot 4D data in a 3D scatter plot on a new device,
    # use PCA to go from 4D to 3D, and add a bounding box and 3D axes
    # decoration (\code{type='d'}) on top of the axes of the 4D space
    # (\code{type='a'}).
    plot3dProj(iris[,1:4], cls=iris$Species, tx=txPca(iris[,1:4]), type='sad', devices=NULL)

    # Plot the iris data set with additional decoration: ellipses
    # representing individual species of the flowers, a box and a text
    # in the center, and a plane.
    iris.setosa<-iris[iris$Species=='setosa',1:3]
    iris.versicolor<-iris[iris$Species=='versicolor',1:3]
    iris.virginica<-iris[iris$Species=='virginica',1:3]
    plot3dProj(iris[,1:3], col=c('red','green','blue')[as.numeric(iris$Species)],
      type='sdpbet', # scatter plot with decoration, a plane, a box,
                     # elippses and a text
      ellipse=list(
        list(cov(iris.setosa),center=colMeans(iris.setosa),col='red',alpha=.1),
        list(cov(iris.versicolor),center=colMeans(iris.versicolor),col='green',alpha=.1),
        list(cov(iris.virginica),center=colMeans(iris.virginica),col='blue',alpha=.1)),
      box=list(center=colMeans(iris[,1:3]),scale=c(.3,.3,.3),col='black',alpha=.2),
      planes=list(0,0,1,-2.5,col='yellow',alpha=.2),
      texts=list(colMeans(iris[,1:3]),text='center',col='black'),
      devices=NULL)

    # Plot two data sets: all Iris flowers (on the left side) and
    # Setosa and Versicolor species only (on the right side).
    # In both cases, produce 2D and 3D spatter plots.
    # For the second data set, alter the size and the alpha value
    # of Setosa flowers. Also, plot small axes under the scatter plots.
    plot3dProj(x = iris[, 1:3], cls = iris$Species,
      main = 'Setosa, Versicolor and Virginica',
      x2 = iris[iris$Species != 'virginica', 1:3],
      cls2 = iris$Species[iris$Species != 'virginica'],
      alpha2 = c(1,.3)[1+(iris$Species[iris$Species != 'virginica']=='setosa')],
      size2 = c(3,10)[1+(iris$Species[iris$Species != 'virginica']=='setosa')],
      main2 = 'Setosa and Versicolor',
      ty='fw,swm,s2m2d,f2;,a',heights=c(2,1),
      devices=NULL) # devices=NULL opens a new 'rgl' device
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
