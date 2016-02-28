\name{countFactorLevels}
\alias{countFactorLevels}
\title{Count the number of factor levels.}

\usage{countFactorLevels(x)}
\arguments{
  \item{x}{a data frame, a list, or a factor}
}

\value{a data frame having the columns of 'x' and two rows ('all',
'used') holding numbers of all (used) factor levels in
individual columns of 'x'}

\author{Tomas Sieger}




\examples{
if (require(MASS)) {
  countFactorLevels(Cars93)

  # omitting the first row results in some levels of 'Cars$Model' and
  # 'Cars93$Make' unused (and reported in the second row)
  countFactorLevels(Cars93[-1L, ])
  }
}
