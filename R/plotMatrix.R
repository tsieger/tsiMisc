plotMatrix<-structure(
function # Plot a matrix.
##description<<
## 'plotMatrix' plots a matrix, decorated with the names
## of rows and columns, and, if requested, with lines separating
## specified rows/columns. Also, a legend gets drawn, by default.
(x, ##<< a matrix
col = gray.colors(16), ##<< colors
transpose = FALSE, ##<< transpose the matrix?
axes = c(2,3), ##<< which axes (sides of the matrix) to annotate?
## (1=below, 2=left, 3=above and 4=right).
legend = TRUE, ##<< draw legend?
legend.width = .2, ##<< the relative width of the legend to the total
## width of the plot
legend.nlevels = length(col) + 1, ##<< number of levels in the legend
## (defaults to the number of colors in 'col')
rowDelim = NULL, ##<< an optional vector of rows after which to put a
## separation line
rowDelimCol = 'black', ##<< the color of the row separation line
colDelim = NULL, ##<< an optional vector of columns after which to put
## a separation line
colDelimCol = 'black' ##<< the color of the column separation line
) {
  xLogical<-FALSE
  if (!is.matrix(x)) {
    x<-as.matrix(x)
  }
  if (is.logical(x)) {
    for (i in rseq(1,ncol(x),1)) x[,i]<-as.numeric(x[,i])
    xLogical<-TRUE
  }
  if (!is.numeric(x)) {
    stop('\'x\' is not a numeric matrix.')
  }

  # setup plot(s)
#.pn(par('mai'))
  opar<-par(mai=c(
    .2+(1%in%axes && !is.null(colnames(x)))*max(.5,max(strwidth(colnames(x),'in'))),
    .1+(2%in%axes && !is.null(rownames(x)))*max(.5,max(strwidth(rownames(x),'in'))),
    .2+(3%in%axes && !is.null(colnames(x)))*max(.5,max(strwidth(colnames(x),'in'))),
    .2+(4%in%axes && !is.null(rownames(x)))*max(.5,max(strwidth(rownames(x),'in')))))
  if (transpose) x<-t(x)
  if (legend && is.numeric(legend.width) && legend.width>0) {
    layout(matrix(c(1,2),1),widths=c(1-legend.width,legend.width))
  } else {
    legend<-FALSE
  }

  # plot the matrix
  image(t(x[nrow(x):1,]),xaxt='n',yaxt='n',col=col)

  if (1%in%axes && !is.null(colnames(x))) axis(1,(1:ncol(x)-1)/(ncol(x)-1),colnames(x),las=3)
  if (3%in%axes && !is.null(rownames(x))) axis(3,(1:ncol(x)-1)/(ncol(x)-1),colnames(x),las=3)
  if (2%in%axes && !is.null(colnames(x))) axis(2,(1:nrow(x)-1)/(nrow(x)-1),rev(rownames(x)),las=1)
  if (4%in%axes && !is.null(rownames(x))) axis(4,(1:nrow(x)-1)/(nrow(x)-1),rev(rownames(x)),las=1)

  # plot delimiters, if requested
  if (!is.null(colDelim)) {
  abline(v=(2*colDelim-1)/2/(ncol(x)-1),col=colDelimCol)
  }
  if (!is.null(rowDelim)) {
    rowDelim<-nrow(x)-rowDelim
    abline(h=(2*rowDelim-1)/2/(nrow(x)-1),col=rowDelimCol)
  }

  par(opar)

  # create legend, if requested
  if (legend) {
    plot.new()
    #opar<-par(mai=par('mai')*c(1,.3,1,1))
    legend.levels<-seq(range(x,finite=TRUE)[1],range(x,finite=TRUE)[2],len=legend.nlevels)
    plot.window(xlim=c(0,1),ylim=range(legend.levels),xaxs="i",yaxs="i")
    rect(0,legend.levels[-length(legend.levels)],1,legend.levels[-1L],
      col=col[round(seq(1,length(col),len=length(legend.levels)-1))])
    axis(4,at=pretty(range(x,finite=TRUE),legend.nlevels))
    #par(opar)
    # restore the single-subfigure layout
    layout(1)
  }

}, ex=function() {
  x<-matrix(1:36,6)
  rownames(x)<-paste('row',1:6)
  colnames(x)<-paste('column',1:6)
  x
  plotMatrix(x)

  # group rows and columns
  x<-repmat(matrix(3-c(1,2,1,2,3,2,1,2,1),3),3,3)
  plotMatrix(x, rowDelim = c(3,6), colDelim = c(3,6), colDelimCol='red')
})
