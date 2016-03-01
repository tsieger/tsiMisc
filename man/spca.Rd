\name{spca}
\alias{spca}
\title{Supervised PCA.}
\description{'spca' computes supervised principal component analysis as
described in Barshan et al.
}
\usage{spca(x, xLabels)}
\arguments{
  \item{x}{a data matrix (features in columns, samples in rows)}
  \item{xLabels}{classification of x (logical or factor)}
}


\references{Barshan, E., Ghodsi, A., Azimifar, Z., Jahromi, M. Z.
_Supervised principal component analysis: Visualization,
classification and regression on subspaces and submanifolds_.
Pattern Recognition, Vol. 44, No. 7. (29 July 2011), pp. 1357-1371,
doi:10.1016/j.patcog.2010.12.015.}
\author{Tomas Sieger}




\examples{
spca(iris[,1:4],iris$Species)
}
