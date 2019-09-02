\name{to.matrix}
\alias{to.matrix}
\title{Conversion to a matrix.}
\description{\code{\link{to.matrix}} attempts to convert any data to a row matrix. In
particular, data frames are converted by \code{\link{as.matrix}}, (numeric)
vectors are converted to a single row/column matrix, according to
the \code{rowMatrix} argument.}
\usage{to.matrix(x, rowMatrix = TRUE)}
\arguments{
  \item{x}{data to convert to a matrix}
  \item{rowMatrix}{if \code{TRUE}, numeric vectors are converted to
a single row matrix, if \code{FALSE}, they get converted to a single
column matrix}
}

\value{matrix}

\author{Tomas Sieger}




\examples{
to.matrix(1:3)
to.matrix(1:3,rowMatrix=FALSE)
to.matrix(iris[1:3,1:4])
to.matrix(matrix(1:4,2))
}
