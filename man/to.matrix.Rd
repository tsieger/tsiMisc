\name{to.matrix}
\alias{to.matrix}
\title{Conversion to a matrix.}
\description{'to.matrix' attempts to convert any data to a row matrix. In
particular, data frames are converted by 'as.matrix', (numeric)
vectors are converted to a single row matrix.}
\usage{to.matrix(x)}
\arguments{
  \item{x}{data to convert to a matrix}
}

\value{matrix}

\author{Tomas Sieger}




\examples{
to.matrix(1:3)
to.matrix(iris[1:3,1:4])
to.matrix(matrix(1:4,2))
}
