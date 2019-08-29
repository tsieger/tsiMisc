\name{strTrim}
\alias{strTrim}
\title{Trim leading and trailing spaces from a string.}
\description{\code{\link{strTrim}} trims leading and trailing spaces from a string.}
\usage{strTrim(x)}
\arguments{
  \item{x}{character string (or a vector of strings) to be trimmed}
}

\value{trimmed string(s)}

\author{Tomas Sieger <tomas.sieger@seznam.cz> [aut, cre]}




\examples{
strTrim(' Hello, World!  ')
strTrim(c('a ', 'b ', ' c '))
}
