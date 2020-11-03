\name{histv.formula}
\alias{histv.formula}
\title{Vertically aligned histograms.}
\description{\code{\link{histv}} creates one or more vertically aligned histograms,
rendered by \code{\link[graphics]{hist}}. This is useful for visual
comparison of several numeric vectors.}
\usage{\method{histv}{formula}(formula, 
    data = NULL, na.action = NULL, 
    drop = FALSE, sep = ".", 
    lex.order = FALSE, 
    xlab = NULL, ..., 
    debug = FALSE)}
\arguments{
  \item{formula}{\code{formula}}
  \item{data}{used in case a \code{formula} interface is used}
  \item{na.action}{
}
  \item{drop}{passed to \code{split.default} in case a \code{formula}
interface is used}
  \item{sep}{passed to \code{split.default} in case a \code{formula}
interface is used}
  \item{lex.order}{passed to \code{split.default} in case a \code{formula}
interface is used}
  \item{xlab}{xlab(s) of the individual histograms. If a
character vector is suplied, it will be used to define the title
of individual histograms, recycled if necessary.}
  \item{\dots}{histv-specific arguments, see \code{histv}}
  \item{debug}{if TRUE, debugs will be printed. If numeric of value
greater than 1, verbose debugs will be produced.}
}



\author{Tomas Sieger}




\examples{
histv(Sepal.Length~Species,iris)
}
