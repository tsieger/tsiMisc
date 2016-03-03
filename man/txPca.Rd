\name{txPca}
\alias{txPca}
\title{PCA transform.}
\description{'txPca' transforms data using principal component analysis.
}
\usage{txPca(x, k = 3, ...)}
\arguments{
  \item{x}{a data matrix (features in columns, samples in rows)}
  \item{k}{number of dimensions of the result, defaults to 3 in order
to be usable in 'plot3dProj'}
  \item{\dots}{additional arguments to 'prcomp'}
}



\author{Tomas Sieger}



\seealso{'prcomp', 'txSpca', 'plot3dProj'}
\examples{
tx<-txPca(iris[,1:4])
plot(tx(iris[1:10,1:4])[,1:2])
}
