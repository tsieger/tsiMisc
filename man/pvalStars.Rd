\name{pvalStars}
\alias{pvalStars}
\title{p-value text summary.}
\description{'pvalStars' creates a graphical representation of p-value(s) (AKA
"significance stars").

sealso<< \link[gtools]{stars.pval}}
\usage{pvalStars(p, width = NULL, 
    right = TRUE)}
\arguments{
  \item{p}{p-value(s)}
  \item{width}{\code{width} of text summary (number of characters), if not
\code{NULL}, the summary gets padded with spaces to consist
of at least \code{width} characters}
  \item{right}{the type of padding}
}

\value{a vector of character strings representing the significance of
'p'}

\author{Tomas Sieger}




\examples{
p<-c(1,.1,.05,.01,.001,.000001,NA)
cbind(p,pvalStars(p))
}
