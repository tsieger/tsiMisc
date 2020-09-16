\name{columnIndexOf}
\alias{columnIndexOf}
\title{Indices of data frame columns identified by name.}
\description{Indices of data frame columns identified by name.}
\usage{columnIndexOf(x, d, regexp = TRUE)}
\arguments{
  \item{x}{name(s) of columns to be searched for}
  \item{d}{data frame whose columns shall be searched}
  \item{regexp}{If TRUE, \code{x} of character type are interpreted as
regular expressions.}
}

\value{indices of requested columns}

\author{Tomas Sieger}




\examples{
# Find indices of 'Petal.Length' and 'Sepal.Length' columns in the
# 'iris' data:
columnIndexOf(c('Sepal.Length','Petal.Length'),iris)

# Alternatively, using regular expressions:
columnIndexOf(c('.*Length'),iris)
}
