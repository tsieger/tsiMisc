\name{blandAltman}
\alias{blandAltman}
\title{Bland-Altman plot.}
\description{The Bland-Altman plot (AKA Tukey mean-difference plot) is a
graphical tool to measure agreement between two estimates of a
single variables methods.
}
\usage{blandAltman(x1, x2, xlab = "Average", 
    ylab = "Difference", 
    alpha = 0.05)}
\arguments{
  \item{x1}{the first estimate}
  \item{x2}{the second estimate}
  \item{xlab}{a label of x-axis on which the mean of the two
estimates is plotted}
  \item{ylab}{a label of y-axis on which the difference
between the two estimates is plotted}
  \item{alpha}{significance level of the confidence interval}
}
\details{Code taken from http://rstats.tiddlyspot.com/#Bland-Altman and
updated slightly.
}
\value{a list of 'mean' (the of the two estimates), 'diff' (the diff of
the estimates, 'se' (the standard error of the differences), and
the '1-alpha/2' confidence intervals for the 'mean +/- 2 * SD of
diffs' given as 'lower' and 'upper'}
\references{Bland JM, Altman DG (1986) _Statistical methods for
assessing agreement between two methods of clinical measurement.__
Lancet 327 (8476): 30710.}
\author{Tomas Sieger}




\examples{
n<-20
x<-rnorm(n)
x1<-x+rnorm(n,0,.1)
x2<-x+rnorm(n,0,.1)
blandAltman(x1,x2)
}
