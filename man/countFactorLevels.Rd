\name{countFactorLevels}
\alias{countFactorLevels}
\title{Count the number of factor levels.}
\description{\code{\link{countFactorLevels}} summarizes a data frame, a list, or a factor by
computing the number of declared and used factor levels in it (in
case of factors) or its constituents (in case of data frames and
lists).}
\usage{countFactorLevels(x)}
\arguments{
  \item{x}{a data frame, a list, or a factor}
}

\value{a data frame having the columns of \code{x} and two rows ('all',
'used') holding numbers of all (used) factor levels in
individual columns of \code{x}}

\author{Tomas Sieger <tomas.sieger@seznam.cz> [aut, cre]}




\examples{
if (require(MASS)) {
  countFactorLevels(Cars93)

  # omitting the first row results in some levels of \code{Cars$Model} and
  # \code{Cars93$Make} unused (and reported in the second row)
  countFactorLevels(Cars93[-1L, ])

  countFactorLevels(Cars93$Model[-1L])
  }
}
