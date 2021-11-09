\name{dfc2num}
\alias{dfc2num}
\title{Convert columns to numeric.}
\description{\code{dfc2num} converts data frame (or matrix) columns to numeric type.}
\usage{dfc2num(x)}
\arguments{
  \item{x}{
}
}



\author{Tomas Sieger}




\examples{
x<-data.frame(a=1:3,b=factor(c('a1','a2','a2')))
dfc2num(x)
}
