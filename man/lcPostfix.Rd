\name{lcPostfix}
\alias{lcPostfix}
\title{Longest common postfix.}
\description{\code{\link{lcPostfix}} returns the longest common postfix of
a vector of strings.
}
\usage{lcPostfix(x, ignore.case = FALSE)}
\arguments{
  \item{x}{a vector of character strings}
  \item{ignore.case}{
}
}

\value{A character string vector having the longest common postfix removed.}

\author{Tomas Sieger}



\seealso{\code{\link{lcPrefix}},\code{\link{rmLcPostfix}}
}
\examples{
lcPostfix(c('abab','cdab'))
}
