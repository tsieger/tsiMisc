\name{projectDataInto1d}
\alias{projectDataInto1d}
\title{Project multidimensional data onto a line.}
\description{Project multidimensional dataset onto a line described by two points (centers).
}
\usage{projectDataInto1d(x, 
    c0, c1)}
\arguments{
  \item{x}{multidimensional data (\code{n} \code{x} \code{k} matrix)}
  \item{c0}{first center (vector in \code{k}-dimensional space)}
  \item{c1}{second center (vector in \code{k}-dimensional space)}
}

\value{A vector of length \code{n} holding the projection of data onto
the line aligned along centers \code{c0} and \code{c1}.
Points in the hyperplane perpendicular to the line and
intersecting center \code{c0} get mapped to \code{0}, points in
the hyperplane perpendicular to the line and intersecting center
\code{c1} get mapped to \code{1}, etc.}

\author{Tomas Sieger}




\examples{
set.seed(1)
x1<-cbind(rnorm(100,-2),rnorm(100))
x2<-cbind(rnorm(100,2),rnorm(100))
p<-projectDataInto1d(rbind(x1,x2),c0=c(-2,0),c1=c(2,0))

layout(rbind(1,2))
plot(rbind(x1,x2),frame=FALSE,main='2D data')
hist(p,breaks=50,main='1D projection')
layout(1)
}
