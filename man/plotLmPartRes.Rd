\name{plotLmPartRes}
\alias{plotLmPartRes}
\title{Plot partial residuals of a simple linear model fit.}
\description{plotLmPartRes shows the relation between selected covariates from a lm model
and the response, as well as the relation of those covariates and the response
adjusted for the other covariates.

The functionality of plotLmPartRes is very limited.
Only numeric vectors are supported (factors, interactions, I()'s are NOT).
\code{\link[car]{crPlot}} and \code{\link[car]{ceresPlot}} should be preferred.
}
\usage{plotLmPartRes(m, covNames = names(m$model)[-1])}
\arguments{
  \item{m}{an object of class 'lm'}
  \item{covNames}{names of covariates to plot (defaults to all of them)}
}



\author{Tomas Sieger}



\seealso{\code{\link[car]{crPlot}}, \code{\link[car]{ceresPlot}}}
\examples{
# prepare data
set.seed(1)
n<-40
h<-rnorm(n)
f<-rnorm(n)
w<-3*h-f+rnorm(n)
d<-data.frame(h,w,f)
# fit lm
m<-lm(f~h+I(w-d),d)
summary(m)
# plot partial residuals
plotLmPartRes(m)
}
