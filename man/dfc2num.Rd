\name{dfc2num}
\alias{dfc2num}
\title{Convert columns to numeric.}
\description{\code{dfc2num} converts data frame (or matrix) columns to numeric type.
Factors get converted to its numeric representation,
textual representation of numbers get converted back to numbers,
other texts get converted to factor and then to its numeric codes,
and other types get converted using \code{as.numeric()}.}
\usage{dfc2num(x)}
\arguments{
  \item{x}{
}
}



\author{Tomas Sieger}




\examples{
x<-data.frame(a=1:3,b=factor(c('a1','a2','a2')))
dfc2num(x)

x<-data.frame(a=1:3,b=c('1','2','2'))
dfc2num(x)

x<-cbind(matrix(1:4,2),c('a','b'))
dfc2num(x)

x<-cbind(c(1,2,NA,Inf),c('a','b','d','c'))
dfc2num(x)

x<-data.frame(num=c(1,2,NA,Inf),text=c('a','b','d','c'))
dfc2num(x)
}
