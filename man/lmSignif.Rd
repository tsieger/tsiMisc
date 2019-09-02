\name{lmSignif}
\alias{lmSignif}
\title{Overall linear model significance.}
\description{\code{\link{lmSignif}} compute the significance of the F statistics in a
\code{\link[stats]{summary.lm}} object.}
\usage{lmSignif(s)}
\arguments{
  \item{s}{an object of class \code{\link[stats]{summary.lm}}
(or a linear model of class \code{\link[stats]{lm}} that will get summarized)}
}

\value{Overall significance of a linear model.}

\author{Tomas Sieger <tomas.sieger@seznam.cz> [aut, cre]}




\examples{
m <- lm(Sepal.Length~Sepal.Width, data=iris[iris$Species=='setosa',])
summary(m)
lmSignif(summary(m))
}
