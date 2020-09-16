\name{spca}
\alias{spca}
\title{Supervised PCA.}
\description{\code{\link{spca}} computes supervised principal component analysis as
described in Barshan et al.
}
\usage{spca(x, y = diag(1, nrow(x)), 
    center = TRUE, scale = FALSE, 
    retx = FALSE, debug = FALSE)}
\arguments{
  \item{x}{a data matrix (features in columns, samples in rows)}
  \item{y}{target classification of \code{x} (logical, numeric,
or a factor), or a kernel matrix of the target. If not specified, it
defaults to identity matrix, in which case SPCA becomes equivalent
to classical PCA (as the matrix being decomposed equals the
covariance matrix of 'x'. (Strictly speaking, when centering is in
use, SPCA becomes the classical PCA. Otherwise, SPCA yields
components similar to those yielded by PCA over centered data, but
shifted.)}
  \item{center}{a logical value indicating whether to \code{center} the
data. This is advisable.}
  \item{scale}{a logical value indicating whether to \code{scale} the
data to have unit variance.}
  \item{retx}{a logical value indicating whether to return the
rotated version of 'x'}
  \item{debug}{if TRUE, debugs will be printed. If numeric of value
greater than 1, verbose debugs will be produced.}
}

\value{Eigenvalue decomposition of \code{Q} (see the paper). The value is a
list of \code{values} and \code{vectors} components (see
\code{\link[base]{eigen}},
\code{Q}, the matrix being decomposed, and \code{center} and \code{scale}
holding the centering and scaling used, or \code{FALSE}.
If \code{retx} is \code{TRUE}, the rotated version of \code{x}
is returned in \code{x}.
The number of eigenvalues and eigenvectors correspond to the
dimension of the output space.}
\references{Barshan, E., Ghodsi, A., Azimifar, Z., Jahromi, M. Z.
_Supervised principal component analysis: Visualization,
classification and regression on subspaces and submanifolds_.
Pattern Recognition, Vol. 44, No. 7. (29 July 2011), pp. 1357-1371,
doi:10.1016/j.patcog.2010.12.015.}
\author{Tomas Sieger}




\examples{
spca(iris[,1:4],iris$Species)
}
