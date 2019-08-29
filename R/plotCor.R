plotCor<-structure(
function # Plot decorated bivariate correlations.
##description<<
## \code{\link{plotCor}} creates a matrix of bivariate correlation plots between
## all/selected pairs of variables in a data frame, similarly to
## \code{plot.data.frame}. On the diagonal, there are histograms of
## individual variables colored, by default,according to the estimated
## normality of those variables. Below (or above) the diagonal, there
## are scatter plots enriched by smoothed conditional estimates of the
## mean. Above (or below) the diagonal, there are estimates of the
## correlation coefficients and their significances shown.
##
## \code{\link{plotCor}} is based on / inspired by several other similar plots.
## I thank their authors, but, unfortunatelly, can't give credits to
## them, as I can't remember all of them.
##
#<<references
## Mohammad F. HUQUE , Mohammed QUASEM, Atiar RAHMAN, and Satya D. DUBEY.
## An Improved Ad-Hoc Multiplicity Adjustment Method for Correlated Response Variables
## Statistics in Biopharmaceutical Research 2010, Vol. 2, No. 1, DOI: 10.1198/sbr.2009.0052
(x, ##<< data frame
method = c('pearson','spearman','spearmanExact','glmPoisson','lm'), ##<< method
## used to compute the coefficient and the significance of the
## relation between each pair of variables. 'pearson', 'spearman', and
## 'spearmanExact' refer to correlation coefficients (the former two
## implemented in terms of 'stats::cor.test', the latter in terms of
## 'coin::spearman_test'). 'lm' refers to a linear model.
## 'glmPoisson' refers to a Poisson generalized linear model, i.e.
## 'stats::glm' with the Poisson family.
adjust = FALSE, ##<< adjust p-values to the number of tests performed?
## If TRUE, approximate adjustment is used, correcting for the
## estimated effective number of tests performed, considering possible
## multicollinearity between variables. The effective number of tests
## is estimated as 'n ^ (1 - meanCorr)', where 'n' is the number of all
## bivariate tests performed and 'meanCor' is the mean absolute value
## of correlations between all pairs of variables considered. (For the
## purpose of this estimation, Pearson correlation is used when
## 'method' is set to 'pearson' or 'lm', and Spearman correlation is
## used otherwise.)
## If FALSE (the default), no adjustment is performed.
## If adjust='full' is used, adjustment to the full number of tests
## performed is employed.
## If 'adjust' is numeric, the number of tests performed is set to this
## value.
## If adjustment is requested, Bonferroni procedure is employed.
i1 = NULL, ##<< optional vector of indices or names of column of 'x' that
## form first elements of correlation pairs. By default, all columns
## are considered.
i2 = NULL, ##<< optional vector of indices or names of column of 'x' that
## form second elements of correlation pairs. By default, all columns
## are considered.
#fileName = NULL, ##<< optional name of
#picWidth=100,picHeight=100,
normalityColor = TRUE, ##<< If TRUE, the diagonal histograms of
## individual variables get colored according to the estimated
## normality of the variables (normality in 'green', non-normality in
## 'red', indeterminate normality in 'gray').
aboveDiag = FALSE, ##<< If TRUE, scatter plots will appear above the
## diagonal and decorations below the diagonal. If FALSE (the default),
## scatter plots will appear below the diagonal and decorations above
## the diagonal.
silent = FALSE, ##<< if TRUE, the number of tests compenstaing for gets
## displayed
plot = TRUE, ##<< if TRUE, a plot is produced. 
... ##<< further arguments passed on to 'pairs'
) {
  panel.cor <- function(x, y, digits=2, prefix="", cex.cor,
    method = c('pearson','spearman','spearmanExact','lm','glmPoisson')) {

    method<-match.arg(method)
    ok<-complete.cases(x,y)
    x<-x[ok]
    y<-y[ok]

    usr <- par("usr"); on.exit(par(usr)) 
    par(usr = c(1,2,1,2))

    if (method=='pearson') {
      simplifiedMethod<-'pearson'
    } else if (method=='spearman') {
      simplifiedMethod<-'spearman'
    } else if (method=='spearmanExact') {
      simplifiedMethod<-'spearman'
    } else if (method=='lm') {
      simplifiedMethod<-'pearson'
    } else if (method=='glmPoisson') {
      simplifiedMethod<-'spearman'
    } else {
      stop('unsupported method "',method,'"')
    }

    # compute P-value of given test ('method')
    if (method=='spearmanExact') {
      p<-tryCatch(pvalue(spearman_test(x~y,data.frame(x=x,y=y))),error=function(e)NA)
      r<-tryCatch(statistic(spearman_test(x~y,data.frame(x=x,y=y))),error=function(e)NA)
    } else if (method=='glmPoisson') {
      p<-tryCatch({
        m<-glm(x~y,family='poisson')
        m0<-glm(x~1,family='poisson')
        anova(m,m0,test='Chisq')[2,5]
      },error=function(e)NA)
      r<-tryCatch({coef(glm(x~y,family='poisson'))[2]},error=function(e)NA)
    } else if (method=='lm') {
      p<-tryCatch({
        coef(summary(lm(x~y)))[2,4]
      },error=function(e)NA)
      r<-tryCatch({coef(lm(x~y))[2]},error=function(e)NA)
    } else {
      # pearson, spearman
      p<-tryCatch(cor.test(x,y,method=method)$p.value,error=function(e)NA)
      r<-cor(x,y,method=method)
    }

    # compute correlation coefficient based on 'simplifiedMethod'
    #r<-cor(x, y,method=simplifiedMethod)

    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex <- 0.8/strwidth(txt)

    compCount<-.gfc(plotCorData.adjustmentCnt)

    p<-compensatePValue(p,compCount,'bonferroni')

    # borrowed from printCoefmat
    signif <- symnum(p, corr = FALSE, na = FALSE,
      cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
      symbols = c("***", "**", "*", ".", " "))

    text(1.5, 1.5, txt, cex = max(.4,cex * abs(r)))
    text(1.5, 1.8, signif, cex=cex, col=2)
  }
  panel.cor.pearson <- function(...) panel.cor(...,method='pearson')
  panel.cor.spearman <- function(...) panel.cor(...,method='spearman')
  panel.cor.spearmanExact <- function(...) panel.cor(...,method='spearmanExact')
  panel.cor.glmPoisson <- function(...) panel.cor(...,method='glmPoisson')
  panel.cor.lm <- function(...) panel.cor(...,method='lm')

  panel.hist <- function(x, ...) {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, ...)
  }

  panel.histWithNormalityColorCodes <- function(x, ...) {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col=c('green','red','gray')[normality2flag(x)],...)
  }

  method<-match.arg(method)
  if (!is.null(i1)) {
    if (is.character(i1)) {
      i1<-indexOf(i1,colnames(x))
      if (length(i1)==0) stop('\'i1\' does not index any columns')
    }
    if (!is.numeric(i1) || any(i1<1) || any(i1>ncol(x))) stop('invalid \'i1\' argument')
  }
  if (!is.null(i2)) {
    if (is.character(i2)) {
      i2<-indexOf(i2,colnames(x))
      if (length(i2)==0) stop('\'i2\' does not index any columns')
    }
    if (!is.numeric(i2) || any(i2<1) || any(i2>ncol(x))) stop('invalid \'i2\' argument')
  }
  if (is.null(i1) != is.null(i2)) stop('only one of \'i1\' and \'i2\' arguments supplied')

  #if (plot && !is.null(fileName)) {
  #    png(fileName,width=picWidth*ncol(x),height=picHeight*ncol(x))#,res=50)
  #    on.exit(dev.off())
  #}
  simplifiedMethod<-method
  if (method=='pearson') {
      panel<-panel.cor.pearson
  }
  else if (method=='spearman') {
      panel<-panel.cor.spearman
  }
  else if (method=='spearmanExact') {
      panel<-panel.cor.spearmanExact
      simplifiedMethod<-'spearman'
  } else if (method=='glmPoisson') {
      panel<-panel.cor.glmPoisson
      simplifiedMethod<-'spearman'
  } else if (method=='lm') {
      panel<-panel.cor.lm
      simplifiedMethod<-'pearson'
  }
  else {
      stop('unsupported correlation type')
  }

  # adjust the statistical significance level
  # by the effective number of independent comparisons

  if (is.null(i1)) {
    n<-(nrow(x)*(nrow(x)-1))/2
  } else {
    # estimate the number of tests made
    #     2 3 4 5         2  3        2 3 4 5            4 5          4 5
    # 1   y y y y                  1  y y y y      1     y y     1    y y
    # 2   n y y y  =   2  x y   +              +   2     y y  -
    # 3   n n y y      3  x x                      3     y y
    #
    # y = standalone test
    # x = repeated/invalid(trivial) test
    n<-sum(i1%in%i2)*(sum(i1%in%i2)-1)/2 + sum(!(i1%in%i2))*length(i2) +
      sum(!(i2%in%i1))*length(i1) - sum(!(i1%in%i2))*sum(!(i2%in%i1))
  }
  if (is.null(adjust)) adjust<-FALSE
  if (is.logical(adjust)) {
    if (adjust) {
      # adjust according to the effective number of tests
      corrs<-cor(na.omit(x),method=simplifiedMethod)
      if (is.null(i1)) {
        meanCorr<-mean(abs(corrs[upper.tri(corrs)]))
      } else {
        # We'are interested in correlations between 'i1' and 'i2',
        # so adjust just to these.
        # logical matrix of entries of interest:
        i1i2<-matrix(FALSE,ncol(x),ncol(x))
        i1i2[i1,i2]<-TRUE
        meanCorr<-mean(abs(corrs[upper.tri(corrs)&i1i2]))
      }
      # estimate the effective number of tests made
      n<-n^(1-meanCorr)
    } else {
      # no adjustment
      n<-1
    }
  } else if (is.character(adjust)) {
    if (adjust=='full') {
      # full adjustment
      # leave 'n' as is - it already holds the number of tests
    } else {
      stop('unsupported adjustment')
    }
  } else if (is.numeric(adjust)) {
    # explicit adjustment
    n<-adjust
  } else stop('invalid \'adjust\' argument')
  if (!silent) cat(sprintf('adjusting to %.2f tests\n',n))

  # set up multiple comparison count
  plotCorData.adjustmentCnt<-n
  if (plot) {
      if (normalityColor) dp<-panel.histWithNormalityColorCodes else dp<-panel.hist
      if (aboveDiag) {
          pairs(x,lower.panel=panel,upper.panel=panel.smooth,diag.panel=dp,i1=i1,i2=i2,...)
      } else {
          pairs(x,lower.panel=panel.smooth,upper.panel=panel,diag.panel=dp,i1=i1,i2=i2,...)
      }
  }
  return(invisible(n))
  ### the estimated effective number of the tests performed
}, ex=function() {
  #
  plotCor(iris[,1:4])
  plotCor(iris[,1:4],i1=c('Sepal.Length','Sepal.Width'),i2=c('Petal.Length','Petal.Width'))

  # TODO: demonstrate more features
})
