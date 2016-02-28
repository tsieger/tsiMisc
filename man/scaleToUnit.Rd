\name{scaleToUnit}
\alias{scaleToUnit}
\title{Scale the columns of a numeric matrix to the unit range.}

\usage{scaleToUnit(x, min = 0, max = 1, singular = TRUE)}
\arguments{
  \item{x}{numeric matrix}
  \item{min}{minimum in each dimension to scale to  (defaults to 0)}
  \item{max}{maximum in each dimension to scale to (defaults to 1)}
  \item{singular}{if TRUE, constant columns will be transformed to
the mean of 'min', 'max', not to NaN, as would result from a
straighforward implementation}
}

\value{Scaled version of 'x'. The desired minimum and maximum values
are returned as attributes 'min' and 'max', respectively. The
minimum/maximum values of 'x' are returned as attributes 'min_x'
and 'max_x', respectively. The function used to transform a row
in 'x' to the desired range is returned as 'tx' attribute.}

\author{Tomas Sieger}




\examples{
(x <- scaleToUnit(iris[, 1:4]))
summary(x)

x[1, ]
attr(x, 'scaleToUnit:tx')(iris[1, 1:4])

scaleToUnit(cbind(1:4,1),min=0,max=1)
}
