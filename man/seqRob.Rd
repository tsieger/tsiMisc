\name{seqRob}
\alias{seqRob}
\title{Robust sequence generation resembling the matlab ':' operator.}
\description{'seqRob' is similar to 'base::seq' with the only difference in the
case when 'by' is specified and goes in the opposite direction to
'from' to 'to'. In this case 'base::seq' raises an error, while
'seqRob' returns an empty sequence.}
\usage{seqRob(from = 1, to = 1, by = NULL, ...)}
\arguments{
  \item{from}{from}
  \item{to}{to}
  \item{by}{increment}
  \item{\dots}{
}
}

\value{Sequence similar to the result of 'base::seq'.}

\author{Tomas Sieger}




\examples{
seqRob(from = 1, to = 0, by = 1)
}
