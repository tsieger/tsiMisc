\name{lcPrefix}
\alias{lcPrefix}
\title{Longest common prefix.}
\description{\code{\link{lcPrefix}} returns the longest common prefix of
a vector of strings.
}
\usage{lcPrefix(x, ignore.case = FALSE)}
\arguments{
  \item{x}{a vector of character strings}
  \item{ignore.case}{
}
}
\details{The implementation adapted after 
\url{https://stackoverflow.com/questions/28273716/r-implementation-for-finding-the-longest-common-starting-substrings-in-a-set-of}
}
\value{The longest common prefix.}
\references{\url{https://stackoverflow.com/questions/28273716/r-implementation-for-finding-the-longest-common-starting-substrings-in-a-set-of}
}
\author{Tomas Sieger}



\seealso{\code{\link{lcPostfix}},\code{\link{rmLcPrefix}}
}
\examples{
lcPrefix(c('abcd','abef','aqwe'))
}
