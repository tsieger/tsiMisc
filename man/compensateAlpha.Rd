\name{compensateAlpha}
\alias{compensateAlpha}
\title{Significance level compensation for multiple comparisons.}
\description{\code{\link{compensateAlpha}} compensates statistical significance level
(\code{alpha}) for multiple comparisons/tests.
}
\usage{compensateAlpha(alpha, 
    n, method = c("bonferroni", 
        "sidak"), r = 0)}
\arguments{
  \item{alpha}{significance level to be compensated}
  \item{n}{comparison count}
  \item{method}{compensation \code{method},
'bonferroni' is more conservative, but does not assume
independence of the tests,
'sidak' is more liberal and assumes independence of the tests}
  \item{r}{optional mean "correlation between the tests made",
the extreme value of 0 leads to full compensation, the extreme value
of 1 leads to no compensation (as all the tests are considered equal).
See the SISA help for explanation.}
}

\value{Compensated significance level. It equals \code{alpha/n} for the
\code{'bonferroni'} \code{method}, and \code{1-(1-alpha)^(1/n))} for the \code{'sidak'}
\code{method}.}
\references{SISA, \url{http://www.quantitativeskills.com/sisa/calculations/bonhlp.htm}}
\author{Tomas Sieger}



\seealso{\code{\link{compensatePValue}}, \code{\link[stats]{p.adjust}}}
\examples{
compensateAlpha(.05,2,method='sidak')
}
