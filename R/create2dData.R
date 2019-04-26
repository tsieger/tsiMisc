create2dData <- structure(
function # Create 2D data set interactively.
##description<<
## 'create2dData' allows you to interactively create a 2D data set
## by clicking points in 2D.
##
##details<<
## The user places points in 2D by clicking in a graphical device, and
## gets coordinates of the clicked points.
##
## For the usual `X11' device the identification process is
## terminated by pressing any mouse button other than the first.  For
## the `quartz' device the process is terminated by pressing either
## the pop-up menu equivalent (usually second mouse button or
## `Ctrl'-click) or the `ESC' key.
##
##seealso<< 'identify'
(xyRanges = c(0,1,0,1), ##<< ranges of 2D space (a numeric vector of length 4
## giving the xmin, xma, ymin, and ymax limits)
setupDevice = TRUE, ##<< logical flag specifying whether to setup the graphical device.
## Setting to FALSE enables to call `create2dData' several times
## getting incrementally more and more points.
verb = 1,##<< verbosity level
... ##<< optinal parameters passed to `points'
)
{
  if (setupDevice) {
    if (!is.numeric(xyRanges) || length(xyRanges)!=4) {
      stop('invalid \'ranges\' argument')
    }
    # setup device
    plot(xyRanges[1:2],xyRanges[3:4],ty='n')
  }

  x<-y<-c()
  while (TRUE) {
    p<-locator(1)
    if (is.null(p)) break
    if (verb) cat(paste0('point ',length(x)+1,': ',p$x,' ',p$y,'\n'))
    points(p$x,p$y,...)
    x<-c(x,p$x)
    y<-c(y,p$y)
  }

  return(list(x=x,y=y))
  ### a list of `x' and `y', each holding the coordinates of selected
  ### points
},ex=function() {
  # create first sample
  xy<-create2dData()
  print(xy)

  # create another sample
  xy2<-create2dData(setupDevice=FALSE,col='red',pch=19)
  print(xy2)
})

