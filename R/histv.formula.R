histv.formula<-structure(
function # Vertically aligned histograms.
##description<<
## \code{\link{histv}} creates one or more vertically aligned histograms,
## rendered by \code{\link[graphics]{hist}}. This is useful for visual
## comparison of several numeric vectors.
(formula, ##<< formula
data = NULL, ##<< used in case a formula interface is used
na.action = NULL, ##<a function which indicates what should happen when
## the data contain \code{NA}'s.  The default is to ignore missing
## values.
drop = FALSE, ##<< passed to \code{split.default} in case a formula
## interface is used
sep = ".",  ##<< passed to \code{split.default} in case a formula
## interface is used
lex.order = FALSE, ##<< passed to \code{split.default} in case a formula
## interface is used
xlab = NULL, ##<< xlab(s) of the individual histograms. If a
## character vector is suplied, it will be used to define the title
## of individual histograms, recycled if necessary.
..., ##<< histv-specific arguments, see \code{histv}
debug = FALSE ##<< if TRUE, debugs will be printed. If numeric of value
## greater than 1, verbose debugs will be produced.
) {
  # this code was borrowed and adapted from boxplot.formula
  if (missing(formula) || (length(formula) != 3L)) {
    stop("'formula' missing or incorrect")
  }
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame()))) {
    m$data <- as.data.frame(data)
  }
  m$... <- m$drop <- m$sep <- m$lex.order <- m$xlab <- NULL
  m$na.action <- na.action # force use of default for this method
  ## need stats:: for non-standard evaluation
  m[[1L]] <- quote(stats::model.frame.default)
  mf <- eval(m, parent.frame())
  response <- attr(attr(mf, "terms"), "response")
  if (missing(xlab)) xlab<-names(mf)[response]
  histv(split(mf[[response]], mf[-response],
    drop = drop, sep = sep, lex.order = lex.order),
	xlab = xlab, ..., debug=debug)
},ex=function() {
  histv(Sepal.Length~Species,iris)
})
