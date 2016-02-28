\name{blandAltman}
\alias{blandAltman}
\title{Bland-Altman plot.}

\usage{blandAltman(x1, x2, xlab = "Average", ylab = "Difference", alpha = 0.05)}
\arguments{
  \item{x1}{the first estimate}
  \item{x2}{the second estimate}
  \item{xlab}{a label of x-axis on which the mean of the two
estimates is plotted}
  \item{ylab}{a label of y-axis on which the difference
between the two estimates is plotted}
  \item{alpha}{significance level of the confidence interval}
}

\value{a list of 'mean' (the of the two estimates), 'diff' (the diff of
the estimates, 'se' (the standard error of the differences), and
the '1-alpha/2' confidence intervals for the 'mean +/- 2 * SD of
diffs' given as 'lower' and 'upper'}

\author{Tomas Sieger}




\examples{
n<-20
x<-rnorm(n)
x1<-x+rnorm(n,0,.1)
x2<-x+rnorm(n,0,.1)
blandAltman(x1,x2)
}
