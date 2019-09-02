\name{iclass}
\alias{iclass}
\title{Determine implicit class.}
\description{\code{\link{iclass}} determines the implicit class of a base type.
The code is taken from the great "Advanced R" by Hadley Wickham.
}
\usage{iclass(x)}
\arguments{
  \item{x}{base type}
}

\value{A vector describing the implicit class of 'x'.}
\references{Wickham, Hadley. _Advanced R_.
Available online at \url{http://adv-r.had.co.nz/OO-essentials.html}}
\author{Tomas Sieger}




\examples{
iclass(matrix(1:5))
#> [1] "matrix"  "integer" "numeric"

iclass(array(1.5))
#> [1] "array"   "double"  "numeric"
}
