\name{lecdf}
\alias{lecdf}
\title{Detrend data using local e.c.d.f.}
\description{Transform data by removing the trend from them and convert them to
local e.c.d.f. (empirical cumulative distribution function).
Each element of \code{y} gets transformed into a number in the unit interval,
values near \code{0} represent relatively low values of \code{y} (in
the context of their neighbourhood), values near \code{1} represent
relatively high values of \code{y}.}
\usage{lecdf(x, y, bandwidth = 0.2)}
\arguments{
  \item{x}{vector of x-coordinates}
  \item{y}{vector of y-coordinates}
  \item{bandwidth}{\code{bandwidth} defining the neighbourhood in which to evaluate the quantiles}
}

\value{A vector of local quantiles.}

\author{Tomas Sieger}




\examples{
set.seed(1)
n<-100
x<-runif(n,0,10)
y<-sin(x)+runif(n)
layout(rbind(1,2))
# color points according to their vertical position
# considering their local neighbourhood
l<-lecdf(x,y,bandwidth=.1)
plot(x,y,pch=19,col=topo.colors(100)[rank(l)],main='bandwidth=.1')
# use larger bandwidth
l<-lecdf(x,y,bandwidth=.5)
plot(x,y,pch=19,col=topo.colors(100)[rank(l)],main='bandwidth=.5')
layout(1)
}
