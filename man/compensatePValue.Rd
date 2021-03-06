\name{compensatePValue}
\alias{compensatePValue}
\title{P-value compensation for multiple comparisons.}
\description{**OBSOLETED**
\code{\link{compensatePValue}} compensates a p-value for the number of tests
performed.
}
\usage{compensatePValue(p, n = length(p), 
    method = c("bonferroni", 
        "sidak", "holm-bonferroni"), 
    r = 0)}
\arguments{
  \item{p}{p-value(s) to be compensated}
  \item{n}{number of tests made}
  \item{method}{compensation \code{method},
'bonferroni' is conservative, does not assume independence
of the tests, and controls for the family-wise error (FWE),
'sidak' is more liberal, and controls for FWE only under the
assumption of independence of the tests performed,
'holm-bonferroni' is an iterative procedure controlling for FWE}
  \item{r}{optional mean "correlation between the tests made",
the extreme value of 0 leads to full compensation, the extreme value
of 1 leads to no compensation (as all the tests are considered equal).
See the SISA help for explanation. Note that the FWE is controlled
in the strict sense only for 'r=0'.}
}

\value{Compensated p-value. It equals \code{min(1,p*n)} for the \code{'bonferroni'}
\code{method}, \code{1-(1-p)^n} for the \code{'sidak'} \code{method}, and the result of the
iterative procedure for the \code{method} of \code{'holm-bonferroni'}.}
\references{SISA, \url{http://www.quantitativeskills.com/sisa/calculations/bonhlp.htm}
Sture Holm, A Simple Sequentially Rejective Multiple Test Procedure, Scand J Statist 6: 65-70, 1979}
\author{Tomas Sieger}
\note{deprecated, not working properly, use \code{\link[stats]{p.adjust}} instead
}


\seealso{\code{\link[stats]{p.adjust}}, \code{\link{compensateAlpha}}}
\examples{
# demonstrate the difference between Bonferroni and Sidak:
compensatePValue(.025,2,method='bonferroni')
compensatePValue(.025,2,method='sidak')

# demonstrate the iterative Holm-Bonferroni method:
#compensatePValue(c(.01,.02,.04,.05),method='holm-bonferroni')
#compensatePValue(c(.05,.04,.01,.02),method='holm-bonferroni')
# use 'stats::p.adjust' instead
}
