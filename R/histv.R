histv<-structure(
function # Vertically aligned histograms.
##description<<
## \code{\link{histv}} creates one or more vertically aligned histograms,
## rendered by \code{\link[graphics]{hist}}. This is useful for visual
## comparison of several numeric vectors.
(..., ##<< one or more numeric vectors to compare
setup = TRUE, ##<< if TRUE, graphic device will be set up to hold
## enough subfigures. If FALSE, no setup will be performed.
setupDev = NULL, ##<< deprecated, use \code{setup} instead
ylimFixed = FALSE, ##<< if TRUE, ylim of all histograms will be the
## same. Useful to visually assess the proportions of the individual
## samples.
col = NULL, ##<< colour(s) to be used to fill the bars. The default of
## \code{NULL} yields unfilled bars. If a vector is suplied, it will be
## used to define the color of individual histograms, recycled if
## necessary.
border = NULL, ##<< the color(s) of the border around the bars. The
## default is to use the standard foreground color. If a vector is
## suplied, it will be used to define the border of individual
## histograms, recycled if necessary.
main = NULL, ##<< the title(s) of the individual histograms. If a
## character vector is suplied, it will be used to define the title
## of individual histograms, recycled if necessary.
xlab = NULL, ##<< xlab(s) of the individual histograms. If a
## character vector is suplied, it will be used to define the title
## of individual histograms, recycled if necessary.
callback = NULL, ##<< callback function to be called after each
## individual histogram gets plotted, receiving two arguments: the
## list of arguments passed to \code{\link[graphics]{hist}} previously,
## and the histogram created by the \code{\link[graphics]{hist}}.
## The code can decorate or adapt the plot
## in a histogram-specific way (e.g. use pretty y axis labels).
## it
debug = FALSE ##<< if TRUE, debugs will be printed. If numeric of value
## greater than 1, verbose debugs will be produced.
) UseMethod("histv")
,ex=function() {
  # two aligned histograms with free 'ylim'
  histv(seq(1,10), seq(1,20,.1))
  # two aligned histograms with the same 'ylim' showing the relative
  # proportions
  histv(seq(1,10), seq(1,20,.1), ylimFixed=TRUE)

  # 3 data arguments
  set.seed(1)
  n <- 1000
  a <- rnorm(n)
  b <- runif(n)
  c <- rchisq(n,1)
  histv(a, b, c, breaks = 100, main = c('N(0,1)', 'U(0,1)', 'Chisq(1)'),
    col = c('gray','blue','red'))

  # 1 single list argument holding 3 data vectors
  histv(list(a=a, b=b, c=c), breaks = 100, main = c('N(0,1)', 'U(0,1)', 'Chisq(1)'),
    col = c('gray','blue','red'))

  # formula interface
  histv(Sepal.Length~Species, iris)

})
