\name{txSpca}
\alias{txSpca}
\title{Supervised PCA transform.}
\description{'txSpca' transforms data using supervised principal component
analysis.
}
\usage{txSpca(x, xLabels, k = 3, ...)}
\arguments{
  \item{x}{a data matrix (features in columns, samples in rows)}
  \item{xLabels}{classification of x (logical or factor)}
  \item{k}{number of dimensions of the result, defaults to 3 in order
to be usable in 'plot3dProj'}
  \item{\dots}{additional arguments to 'spca'}
}



\author{Tomas Sieger}



\seealso{'spca', 'txPca', 'plot3dProj'}
\examples{
tx<-txSpca(iris[,1:4],iris$Species)
plot(tx(iris[1:10,1:4])[,1:2])

# comparison of PCA vs. SPCA
# TODO
opar<-par(mfrow=c(1,2))
plot(txSpca(iris[,1:4],iris$Species)(iris[,1:4])[,1:2],col=c('red','green','blue')[as.numeric(iris$Species)])
plot(txPca(iris[,1:4])(iris[,1:4])[,1:2],col=c('red','green','blue')[as.numeric(iris$Species)])
par(opar)
}
