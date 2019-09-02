\name{flip}
\alias{flip}
\title{Flip an array.}
\description{Flip an array along some dimension(s), e.g. flip a matrix upside
down or left to right.}
\usage{flip(x, dim = 1)}
\arguments{
  \item{x}{an array}
  \item{dim}{dimension(s) along which to flip (in sequence), defaults
to 1 (i.e. flip rows).}
}

\value{The array \code{x} having the \code{dim} dimension flipped.}

\author{Tomas Sieger}




\examples{
# flip a matrix
x<-matrix(1:6,2)
x
# flip upside down
flip(x,1)
# flip left to right
flip(x,2)
# flip both upside down and left to right
flip(x,1:2)

# flip a vector
v<-1:10
v
flip(v)

# flip an array
a<-array(1:prod(2:4),2:4)
a
# flip along dim 1
flip(a,1)
# flip along dim 2
flip(a,2)
# flip along dim 3
flip(a,3)
}
