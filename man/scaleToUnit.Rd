\name{scaleToUnit}
\alias{scaleToUnit}
\title{Scale to unit range.}
\description{\code{\link{scaleToUnit}} scales the columns of a numeric matrix to
the unit range.}
\usage{scaleToUnit(x, min = 0, 
    max = 1, solveSingular = TRUE)}
\arguments{
  \item{x}{a numeric matrix}
  \item{min}{the minimum in each dimension to scale to (defaults to
\code{0})}
  \item{max}{the maximum in each dimension to scale to (defaults to
\code{1})}
  \item{solveSingular}{if \code{TRUE}, constant columns will be
transformed to the mean of \code{min} and \code{max}, not to
\code{NaN}, as would result from a straighforward implementation}
}

\value{Scaled \code{x}. The desired minimum and maximum values
are returned as attributes \code{min} and \code{max},
respectively. The minimum/maximum values of \code{x} are returned
as attributes \code{min_x} and \code{max_x}, respectively. The
function used to transform a row in \code{x} to the desired range
is returned as the \code{tx} attribute. The inverse transform can
be find in the \code{txInv} attribute.}

\author{Tomas Sieger}




\examples{

# scale the \\code{iris} data set
x <- iris[, 1:4]
summary(x)
x2 <- scaleToUnit(x)
summary(x2)

# transform explicitly:
x2[1, ]
attr(x2, 'tx')(iris[1, 1:4])

# inverse transform:
y <- cbind(1:4,1)
y
y2 <- scaleToUnit(y ,min = 0, max = 1)
y2
y3 <- attr(y2, 'txInv')(y2)
y3

}
