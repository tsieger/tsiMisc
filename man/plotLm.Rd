\name{plotLm}
\alias{plotLm}
\title{Plot diagnostics of a linear model fit.}
\description{plotLm creates a 2x2 plot holding the 4 diagnoostic plots of a linear model.
It is simply a shortcut for diving the current plot into 2x2 subplots and
plotting the 4 plots.
}
\usage{plotLm(m)}
\arguments{
  \item{m}{an object of class 'lm'}
}



\author{Tomas Sieger}




\examples{
set.seed(1)
x<-rnorm(10)
d<-data.frame(x=x,y=2*x+rnorm(10))
m<-lm(y~x,d)

plotLm(m)
}
