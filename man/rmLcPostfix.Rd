\name{rmLcPostfix}
\alias{rmLcPostfix}
\title{Remove longest common postfix.}
\description{\code{\link{rmLcPrefix}} removes the longest common postfix
from a vector of strings.
}
\usage{rmLcPostfix(x, ignore.case = FALSE)}
\arguments{
  \item{x}{a vector of character strings}
  \item{ignore.case}{
}
}

\value{A character string vector having the longest common postfix removed.}

\author{Tomas Sieger}



\seealso{\code{\link{rmLcPrefix}}, \code{\link{lcPostfix}}
}
\examples{
rmLcPostfix(c('abab','cdab'))
}
