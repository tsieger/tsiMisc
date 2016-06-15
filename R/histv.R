histv<-structure(
function # Vertically aligned histograms.
##description<<
## 'histv' creates one or more vertically aligned histograms,
## rendered by 'hist'. This is useful for visual comparison of several
## numeric vectors.
(..., ##<< one or more numeric vectors to compare
setup = TRUE, ##<< if TRUE, graphic device will be set up to hold
## enough subfigures. If FALSE, no setup will be performed.
setupDev = NULL, ##<< deprecated, use 'setup' instead
ylimFixed = FALSE, ##<< if TRUE, ylim of all histograms will be the
## same. Useful to visually assess the proportions of the individual
## samples.
col = NULL, ##<< colour(s) to be used to fill the bars. The default of
## 'NULL' yields unfilled bars. If a vector is suplied, it will be
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
## list of arguments passed to 'hist' previously, and the histogram
## created by the 'hist'. The code can decorate or adapt the plot
## in a histogram-specific way (e.g. use pretty y axis labels).
## it
debug = FALSE ##<< if TRUE, debugs will be printed. If numeric of value
## greater than 1, verbose debugs will be produced.
) {
    args<-list(...)
    if (!is.null(args[[1]]) && is.list(args[[1]])) {
      args<-args[[1]]
    }
    if (!is.null(setupDev)) {
      warning('\'setupDev\' argument is deprecated, use \'setup\' instead')
      setup<-setupDev
    }
    if (debug) .pn(border)
    if (debug>1) .pn(args)
    if (debug) .pn(names(args))
    if (debug) .pn(length(args))
    # unnamed arguments are the numeric vectors
    if (is.null(names(args))) {
      xs.idx<-rep(TRUE,length(args))
    } else {
      xs.idx<-sapply(names(args),nchar)==0
    }
    if (debug) .pn(xs.idx)
    xs<-args[xs.idx]
    # see http://stackoverflow.com/questions/5754367/using-substitute-to-get-argument-name-with
    xsNames<-sapply(as.list(substitute(list(...)))[-1][xs.idx],deparse)
    n<-length(xs)
    if (debug) .pn(n)
    if (debug>1) .pn(xs)
    if (debug) .pn(xsNames)
    # named arguments are optional arguments to hist()
    opts<-args[sapply(names(args),nchar)>0]
    if (debug) .pn(opts)
    opts<-opts[!((names(opts)%in%'plot'))]
    if (debug) .pn(opts)
    if (debug) .pn(length(opts))
    if (debug>1) .pn(unlist(xs))

    # determine breaks for all the histograms by calling 'hist' over
    # all the data
    a<-list(x=unlist(xs),plot=FALSE)
    if (debug>1) .pn(a)
    a<-c(a,opts)
    if (debug>1) .pn(a)
    hOverall<-suppressWarnings(do.call('hist',a))
    if (ylimFixed) {
      ylimRange<-c()
      for (i in seq(along=xs)) {
        a<-list(
          x=xs[[i]],
          breaks=hOverall$breaks,
          plot=FALSE)
        # add more optional arguments, excluding 'breaks'
        # that have been added already
        if (any(regexpr('^br',names(opts))==-1)) {
          a<-c(a,opts[regexpr('^br',names(opts))==-1])
        }
        if (debug>1) .pn(a)
        tmp<-suppressWarnings(do.call('hist',a))
        ylimRange<-range(ylimRange,tmp$counts)
      }
    }

    if (setup) {
      opar<-par(no.readonly=TRUE)
    }
    hists<-NULL
    tryCatch({
      if (setup) par(mfcol=c(n,1))
        for (i in seq(along=xs)) {
          a<-list(
            x=xs[[i]],
            breaks=hOverall$breaks,
            main=ifelse(is.null(main),paste('Histogram of',xsNames[i]),main[1+(i-1)%%length(main)]),
            xlab=ifelse(is.null(xlab),xsNames[i],xlab[1+(i-1)%%length(xlab)])
          )
          if(!is.null(col)) a<-c(a,list(col=col[1+(i-1)%%length(col)]))
          if(!is.null(border)) a<-c(a,list(border=border[1+(i-1)%%length(border)]))
          # add more optional arguments, excluding 'breaks',
          # 'main', and 'xlab' that have been added already
          if (any(regexpr('^(br|col|border|m|xla)',names(opts))==-1)) {
            a<-c(a,opts[regexpr('^(br|col|border|m|xla)',names(opts))==-1])
          }
          if (ylimFixed) {
            a<-c(a,list(ylim=ylimRange))
          }
          if (debug>1) .pn(a)
          h<-do.call('hist',a)
          if (!is.null(callback)) callback(a,h)
          hists<-c(hists,list(h))
        }
    },finally=if(setup) par(opar))
    return(invisible(hists))
    ### a list of objects of class 'histograms' (see
    ### 'graphics::hist'). The elements correspond to individual
    ### histograms depicting the individual numeric vectors.
},ex=function() {
  # two aligned histograms with free 'ylim'
  histv(seq(1,10), seq(1,20,.1))
  # two aligned histograms with the same 'ylim' showing the relative
  # proportions
  histv(seq(1,10), seq(1,20,.1), ylimFixed=TRUE)

  set.seed(1)
  n <- 1000
  a <- rnorm(n)
  b <- runif(n)
  c <- rchisq(n,1)
  histv(a, b, c, breaks = 100, main = c('N(0,1)', 'U(0,1)', 'Chisq(1)'),
    col = c('gray','blue','red'))
})
