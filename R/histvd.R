histvd<-structure(
function # Vertically aligned histograms.
##description<<
## \code{\link{histvd}} creates one or more vertically aligned histograms,
## rendered by \code{\link[graphics]{hist}}. This is useful for visual
## comparison of several numeric vectors.
(..., ##<< one or more numeric vectors to compare
breaks = 100, ##<< histogram breaks, see 'hist'
col = 'gray' ##<< colour(s) to be used to fill the bars. The default
) {
  histv(...,breaks=breaks,col=col)
  ### a list of objects of class \code{'histograms'} (see
  ### \code{\link{histv}})
},ex=function() {
  # two aligned histograms with free 'ylim'
  histvd(seq(1,10), seq(1,20,.1))
  # two aligned histograms with the same 'ylim' showing the relative
  # proportions
  histvd(seq(1,10), seq(1,20,.1), ylimFixed=TRUE)
})
