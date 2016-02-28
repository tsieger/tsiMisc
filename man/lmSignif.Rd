\name{lmSignif}
\alias{lmSignif}
\title{Overall linear model significance.}

\usage{lmSignif(s)}
\arguments{
  \item{s}{an object of class 'lm.summary'
(or a linear model of class 'lm' that will get summarized)}
}

\value{Overall significance of a linear model.}

\author{Tomas Sieger}




\examples{
m <- lm(Sepal.Length~Sepal.Width, data=iris[iris$Species=='setosa',])
summary(m)
lmSignif(summary(m))
}
