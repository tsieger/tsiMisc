\name{plotShadow}
\alias{plotShadow}
\title{Plot a shadow matrix.}
\description{\code{\link{plotShadow}} explores missing values in a matrix by
plotting the shadow matrix of the data, in which missing values
are coded as black rectangles, and non-missing values are given
in light gray.}
\usage{plotShadow(x, col = gray.colors(2), transpose = FALSE)}
\arguments{
  \item{x}{a matrix}
  \item{col}{color palette}
  \item{transpose}{transpose the matrix?}
}



\author{Tomas Sieger}




\examples{
x<-matrix(1:36,6)
rownames(x)<-paste('row',1:6)
colnames(x)<-paste('column',1:6)
x[2,3:5]<-NA
x[6,6]<-NA
x
plotShadow(x)
}
