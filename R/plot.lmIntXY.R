plot.lmIntXY<-structure(
function # Plot confidence and prediction intervals for regression.
##details<<
## 'plot.lmIntXY' adds confidence and/or prediction intervals to
## an existing plot depicting a simple univariate linear model.
## The intervals are computed using the 'lmInt' function.
##
##seealso<< lmIntXY
##
##note<< deprecated, use 'lmInt' and 'plot.lmInt' instead
##
(x, ##<< an object of class 'lmIntXY'
fit = FALSE, ##<< plot the fitted regression line?
cia = FALSE, ##<< plot confidence intervals \emph{around} the regression
## line? (See 'lmInt' for explanation.)
cif = FALSE, ##<< plot confidence intervals \emph{for} the regression
## line? (See 'lmInt' for explanation.)
pi = FALSE, ##<< plot prediction intervals? (See 'lmInt' for
## explanation.)
... ##<< additional arguments to be passed to the 'lines' function used
## to plot the lines
) {
  if (fit) {
    lines(x$x,x$fit,...)
  }
  if (cia) {
    lines(x$x,x$ciaLwr,...)
    lines(x$x,x$ciaUpr,...)
  }
  if (cif) {
    lines(x$x,x$cifLwr,...)
    lines(x$x,x$cifUpr,...)
  }
  if (pi) {
    lines(x$x,x$piLwr,...)
    lines(x$x,x$piUpr,...)
  }
},ex=function() {
  # see lmIntXY
})
