\name{histvd}
\alias{histvd}
\title{Vertically aligned histograms.}
\description{'histvg' creates one or more vertically aligned histograms,
rendered by 'hist'. This is useful for visual comparison of several
numeric vectors.}
\usage{histvd(..., breaks = 100, col = "gray")}
\arguments{
  \item{\dots}{one or more numeric vectors to compare}
  \item{breaks}{histogram breaks, see 'hist'}
  \item{col}{colour(s) to be used to fill the bars. The default}
}

\value{a list of objects of class 'histograms' (see
'histv')}

\author{Tomas Sieger <tomas.sieger@seznam.cz> [aut, cre]}




\examples{
# two aligned histograms with free 'ylim'
histvd(seq(1,10), seq(1,20,.1))
# two aligned histograms with the same 'ylim' showing the relative
# proportions
histvd(seq(1,10), seq(1,20,.1), ylimFixed=TRUE)
}
