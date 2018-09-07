\name{pvalStars}
\alias{pvalStars}
\title{p-value text summary.}
\description{'pvalStars' creates a graphical representation of p-value(s) (AKA
"significance stars").}
\usage{pvalStars(p)}
\arguments{
  \item{p}{p-value(s)}
}

\value{a vector of character strings representing the significance of
'p'}

\author{Tomas Sieger <tomas.sieger@seznam.cz> [aut, cre]}




\examples{
p<-c(1,.1,.05,.01,.001,.000001,NA)
cbind(p,pvalStars(p))
}
