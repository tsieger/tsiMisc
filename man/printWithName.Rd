\name{printWithName}
\alias{printWithName}
\alias{\code{\link{.pn}}}

\title{Print the name and value of a variable.}
\description{\code{\link{printWithName}} prints both the name and value
of a variable. Useful for debug purposes.
}
\usage{printWithName(x)}
\arguments{
  \item{x}{variable}
}



\author{Tomas Sieger <tomas.sieger@seznam.cz> [aut, cre]}



\seealso{\code{\link[NCmisc]{preview}}, \code{\link[NCmisc]{prv}}.}
\examples{
a<-1:10
.pn(a)
}
