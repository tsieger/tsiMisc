VIF <- structure(
function # Variance-inflaction factor.
##description<<
## \code{\link{VIF}} computes multicolinearity-related diagnostic
## statistics for a linear model with an absolute term.
##
##details<<
## The implementation was taken from Zvara2008.
##
##references<< Karel Zvara: Regrese, Matfyzpress Praha 2008
##
##seealso<< \code{\link[car]{vif}}
(m ##<< linear model fitted by 'lm'
)
{
  if (!is.null(weights(m))) {
    stop("requires unweighted model")
  }
  if (!(any(names(coefficients(m))=="(Intercept)"))) {
    stop("requires model with intercept")
  }
  X0 <- scale(model.matrix(m))[,-1] # standardizace regresoru
  nam <- labels(terms(m))[-1]
  y0 <- scale(m$model[,1]) # standardizace regresandu
  lmobj0 <- lm(y0~X0) # standardizovana regrese
  VIF <- diag(solve(cor(X0)))
  tol <- 1/VIF
  R2 <- 1-tol
  b.star <- coef(lmobj0)[-1]
  out <- cbind(b.star,VIF,R2,tol)
  #rownames(out) <- term.names(m)[-1]
  rownames(out) <- attr(terms(m),'term.labels')
  return(out)
  ### a matrix. In rows: diagnostic statistics related to individual
  ### regressors in the original model.
  ### In columns: regression coefficient estimate in a standardized
  ### model, variance inflation factor, the coefficient of
  ### determination R^2 in model explaining the current regressor using
  ### all the others, and tolerance (1-R^2 = 1/VIF).
},ex=function() {
  require(stats)
  require(datasets)
  m<-lm(Sepal.Width~Sepal.Length+Petal.Width+Petal.Length,iris)
  VIF(m)
})
