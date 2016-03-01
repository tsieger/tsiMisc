\name{compensateAlpha}
\alias{compensateAlpha}
\title{Significance level compensation for multiple comparisons.}
\description{'compensateAlpha' compensates statistical significance level
(alpha) for multiple comparisons/tests.
}
\usage{compensateAlpha(alpha, n, method = c("bonferroni", "sidak"), 
    r = 0)}
\arguments{
  \item{alpha}{significance level to be compensated}
  \item{n}{comparison count}
  \item{method}{compensation method,
'bonferroni' is more conservative, but does not assume
independence of the tests,
'sidak' is more liberal and assumes independence of the tests}
  \item{r}{optional mean "correlation between the tests made",
the extreme value of 0 leads to full compensation, the extreme value
of 1 leads to no compensation (as all the tests are considered equal).
See the SISA help for explanation.}
}

\value{Compensated significance level. It equals 'alpha/n' for the
'bonferroni' method, and '1-(1-alpha)^(1/n))' for the 'sidak
method.}
\references{SISA, http://www.quantitativeskills.com/sisa/calculations/bonhlp.htm}
\author{Tomas Sieger}



\seealso{'compensatePValue', 'stats::p.adjust'}
\examples{
compensateAlpha(.05,2,method='sidak')
}
