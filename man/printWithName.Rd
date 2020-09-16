\name{printWithName}
\alias{printWithName}
\alias{\code{\link{.pn}}}

\title{Print the name and value of a variable.}
\description{\code{\link{printWithName}} prints both the name and value
of a variable. Useful for debug purposes.
}
\usage{printWithName(x, heading = NULL)}
\arguments{
  \item{x}{variable}
  \item{heading}{optional \code{heading} for the variable print}
}



\author{Tomas Sieger}



\seealso{\code{\link[NCmisc]{preview}}, \code{\link[NCmisc]{prv}}.}
\examples{
a<-1:10
.pn(a)
}
