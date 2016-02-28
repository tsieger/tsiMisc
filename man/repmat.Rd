\name{repmat}
\alias{repmat}
\title{Repeat matrix.}

\usage{repmat(x, m, n)}
\arguments{
  \item{x}{a matrix}
  \item{m}{number of repetitions in dimension 1 (row multiply factor)}
  \item{n}{number of repetitions in dimension 2 (column multiply factor)}
}

\value{A matrix of size '(m*r) x (n*c)', where 'r' ('c') represent
the number of rows (columns) of 'x'.}

\author{Tomas Sieger}




\examples{
x<-matrix(1:6,2)
x
repmat(x,2,3)
}
