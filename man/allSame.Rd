\name{allSame}
\alias{allSame}
\title{Given a vector, are all the values in it the same?}

\usage{allSame(x)}
\arguments{
  \item{x}{a vector}
}

\value{TRUE, if all the values are the same (or the vector is empty)}

\author{Tomas Sieger}




\examples{
allSame(c(TRUE, TRUE, TRUE))
allSame(c(TRUE, TRUE, FALSE))
allSame(c(FALSE, FALSE))
allSame(NULL)
}
