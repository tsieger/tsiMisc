\name{rmLcPrefix}
\alias{rmLcPrefix}
\title{Remove longest common prefix.}
\description{\code{\link{rmLcPrefix}} removes the longest common prefix
from a vector of strings.
}
\usage{rmLcPrefix(x, ignore.case = FALSE)}
\arguments{
  \item{x}{a vector of character strings}
  \item{ignore.case}{
}
}

\value{A character string vector having the longest common prefix removed.}

\author{Tomas Sieger}



\seealso{\code{\link{rmLcPostfix}}, \code{\link{lcPrefix}}
}
\examples{
rmLcPrefix(c('abcd','abef','aqwe'))
}
