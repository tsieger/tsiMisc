\name{txPca}
\alias{txPca}
\title{PCA transform.}
\description{\code{txPca} transforms data using principal component analysis.
TODO
}
\usage{txPca(x, k = 3, ...)}
\arguments{
  \item{x}{a data matrix (features in columns, samples in rows)}
  \item{k}{number of dimensions of the result, defaults to 3 in order
to be usable in \code{\link{plot3dProj}}}
  \item{\dots}{additional arguments to \code{\link[stats]{prcomp}}}
}

\value{Transform function taking two arguments: a data matrix \code{y}
to transform, and a logical \code{center} determining whether
the data are to be centered, or not. The parameters of the
transform get returned in the \code{params} attribute (see
\code{\link[stats]{prcomp}}).
In addition, there is the \code{varExplained} function added to
the parameters, which takes \code{k}, the number of components,
and returns the contribution of individual dimensions to the top
\code{k} components.}

\author{Tomas Sieger}



\seealso{\code{\link[stat]{prcomp}}, \code{\link{txSpca}},
\code{\link{plot3dProj}}}
\examples{
tx<-txPca(iris[,1:4])
plot(tx(iris[,1:4])[,1:2],pch=19,col=c('red','green','blue')[as.numeric(iris$Species)])

if (interactive() && require(rgl)) {
  # a 3D example
  x<-iris[,1:4]
  y<-iris$Species
  plot3dProj(x, cls=y, tx=txPca(x))
}
}
