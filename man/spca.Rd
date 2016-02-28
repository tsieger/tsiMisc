\name{spca}
\alias{spca}
\title{Supervised PCA.}

\usage{spca(x, xLabels)}
\arguments{
  \item{x}{a data matrix (features in columns, samples in rows)}
  \item{xLabels}{classification of x (logical or factor)}
}



\author{Tomas Sieger}




\examples{
spca(iris[,1:4],iris$Species)
}
