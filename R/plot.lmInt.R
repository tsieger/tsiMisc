plot.lmInt<-structure(
function # Plot confidence and prediction intervals for regression.
##details<<
## 'plot.lmInt' adds confidence and/or prediction intervals to
## an existing plot depicting a simple univariate linear model.
## The intervals are computed using the 'lmInt' function.
##
##seealso<< lmInt
##
(lmi, ##<< an object of class 'lmInt'
x = colnames(lmi)[1], ##<< the name of dependent variable to plot against
fit = FALSE, ##<< plot the fitted regression line?
cia = FALSE, ##<< plot confidence intervals \emph{around} the regression
## line? (See 'lmInt' for explanation.)
cif = FALSE, ##<< plot confidence intervals \emph{for} the regression
## line? (See 'lmInt' for explanation.)
pi = FALSE, ##<< plot prediction intervals? (See 'lmInt' for
## explanation.)
band = FALSE, ##<< draw polygonal confidence bands instead of simple lines?
... ##<< additional arguments to be passed to the 'lines' function used
## to plot the lines
) {
  fit.arg<-fit
  if (is.character(x)) {
    x<-parse(text=x)
  }
  with(lmi,{
    x<-eval(x)
    xOrder<-order(x)
    x<-x[xOrder]
    if (fit.arg) {
      lines(x,fit[xOrder],...)
    }
    if (cia) {
      if (band) {
        polygon(c(x,rev(x)),c(ciaLwr[xOrder],rev(ciaUpr[xOrder])),...)
      } else {
        lines(x,ciaLwr[xOrder],...)
        lines(x,ciaUpr[xOrder],...)
      }
    }
    if (cif) {
      if (band) {
        polygon(c(x,rev(x)),c(cifLwr[xOrder],rev(cifUpr[xOrder])),...)
      } else {
        lines(x,cifLwr[xOrder],...)
        lines(x,cifUpr[xOrder],...)
      }
    }
    if (pi) {
      if (band) {
        polygon(c(x,rev(x)),c(piLwr[xOrder],rev(piUpr[xOrder])),...)
      } else {
        lines(x,piLwr[xOrder],...)
        lines(x,piUpr[xOrder],...)
      }
    }
  })
},ex=function() {
  # see lmInt
})
