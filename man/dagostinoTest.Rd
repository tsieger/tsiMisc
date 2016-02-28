\name{dagostinoTest}
\alias{dagostinoTest}
\title{D'Agostino normality test.}

\usage{dagostinoTest(x)}
\arguments{
  \item{x}{a non-empty numeric vector of data}
}
\details{Performs D'Agostino normality tests based on skewness, kurtosis, and
an omnibus test.

The implementation is taken from Zvara2008 (appendix A.4.1).
}
\value{Data frame having the results of the skewness, kurtosis, and
omnibus tests in columns, and the test statistics and P-values
in rows.}
\references{DAgostino, Ralph B.; Albert Belanger; Ralph B. DAgostino, Jr
(1990). A suggestion for using powerful and informative tests of normality.
The American Statistician 44 (4): 316-321. JSTOR 2684359.

Karel Zv\'{a}ra: Regrese, Matfyzpress Praha 2008}
\author{Tomas Sieger}




\examples{
dagostinoTest(rnorm(100, mean = 5, sd = 3))
dagostinoTest(runif(100, min = 2, max = 4))
}
