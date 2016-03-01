plot.lmInt<-structure(
function # Plot confidence and prediction intervals for regression.
##description<<
## 'plot.lmInt' adds confidence and/or prediction intervals to
## an existing plot depicting a simple univariate linear model.
## The intervals are computed using the 'lmInt' function.
##
##seealso<< 'lmInt'
##
(x, ##<< an object of class 'lmInt'
xx = colnames(x)[1], ##<< the name of dependent variable to plot against
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
  if (is.character(xx)) {
    xx<-parse(text=xx)
  }
  with(x,{
    xx<-eval(xx)
    xOrder<-order(xx)
    xx<-xx[xOrder]
    if (fit.arg) {
      lines(xx,fit[xOrder],...)
    }
    if (cia) {
      if (band) {
        polygon(c(xx,rev(xx)),c(ciaLwr[xOrder],rev(ciaUpr[xOrder])),...)
      } else {
        lines(xx,ciaLwr[xOrder],...)
        lines(xx,ciaUpr[xOrder],...)
      }
    }
    if (cif) {
      if (band) {
        polygon(c(xx,rev(xx)),c(cifLwr[xOrder],rev(cifUpr[xOrder])),...)
      } else {
        lines(xx,cifLwr[xOrder],...)
        lines(xx,cifUpr[xOrder],...)
      }
    }
    if (pi) {
      if (band) {
        polygon(c(xx,rev(xx)),c(piLwr[xOrder],rev(piUpr[xOrder])),...)
      } else {
        lines(xx,piLwr[xOrder],...)
        lines(xx,piUpr[xOrder],...)
      }
    }
  })
},ex=function() {
  # see lmInt
})
