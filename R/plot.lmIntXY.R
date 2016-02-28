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
(lmi, ##<< an object of class 'lmIntXY'
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
    lines(lmi$x,lmi$fit,...)
  }
  if (cia) {
    lines(lmi$x,lmi$ciaLwr,...)
    lines(lmi$x,lmi$ciaUpr,...)
  }
  if (cif) {
    lines(lmi$x,lmi$cifLwr,...)
    lines(lmi$x,lmi$cifUpr,...)
  }
  if (pi) {
    lines(lmi$x,lmi$piLwr,...)
    lines(lmi$x,lmi$piUpr,...)
  }
},ex=function() {
  # see lmIntXY
})
