\name{panel.cor}
\alias{panel.cor}
\title{Correlation panel for \code{\link[graphics]{pairs}}.}
\description{This is a function that can be passed to
\code{\link[graphics]{pairs}} to visually depict the correlation
coefficient and its significancy.
}
\usage{panel.cor(x, y, digits = 2, 
    prefix = "", cex.cor = NULL, 
    method = c("pearson", 
        "spearman", "spearmanExact", 
        "lm", "glmPoisson"), 
    n.adjust = 1, col.coef = "black", 
    col.signif = "red", 
    dbg = 0, ...)}
\arguments{
  \item{x}{\code{x} data}
  \item{y}{\code{y} data}
  \item{digits}{number of significant \code{digits} in the reported
correlation coefficients}
  \item{prefix}{\code{prefix} to be shown in front of correlation coefficients}
  \item{cex.cor}{magnification for the correlation coefficients and
its significance (if \code{NULL}, is computed automatically and
scaled proportionally to the significance)}
  \item{method}{
}
  \item{n.adjust}{number of tests to adjust P-values to}
  \item{col.coef}{color of coefficient}
  \item{col.signif}{color of significance flags}
  \item{dbg}{debug level}
  \item{\dots}{other arguments (ignored currently)}
}



\author{Tomas Sieger}




\examples{
pairs(iris,
  upper.panel = function(x,y,..) panel.cor(x, y, n.adjust = ncol(iris) * (ncol(iris) - 1) / 2),
  lower.panel = points)
}
