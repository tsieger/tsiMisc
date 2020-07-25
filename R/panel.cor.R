panel.cor <- structure(
function # Correlation panel for \code{\link[graphics]{pairs}}.
##description<<
## This is a function that can be passed to
## \code{\link[graphics]{pairs}} to visually depict the correlation
## coefficient and its significancy.
##
(
x, ##<< x data
y, ##<< y data
digits = 2, ##<< number of significant digits in the reported
## correlation coefficients
prefix = "", ##<< prefix to be shown in front of correlation coefficients
cex.cor = NULL, ##<< magnification for the correlation coefficients and
## its significance (if \code{NULL}, is computed automatically and
## scaled proportionally to the significance)
method = c('pearson','spearman','spearmanExact','lm','glmPoisson'), ##<<
## method of (cor)relation
n.adjust = 1, ##<< number of tests to adjust P-values to
dbg = 0, ##<< debug level
... ##<< other arguments (ignored currently)
) {

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
  if (is.null(cex.cor)) {
    cex <- 0.8/strwidth(txt)
  } else {
    cex <- cex.cor
  }
  if (dbg) .pn(cex)

  if (dbg) .pn(n.adjust)
  if (dbg) .pn(p,'before adjustment')
  p<-p.adjust(p,method='holm',n=n.adjust)
  if (dbg) .pn(p,'after adjustment')

  # borrowed from printCoefmat
  signif <- symnum(p, corr = FALSE, na = FALSE,
    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
    symbols = c("***", "**", "*", ".", " "))

  text(1.5, 1.5, txt, cex = max(.4,cex * abs(r)))
  text(1.5, 1.8, signif, cex=cex, col=2)
},ex=function() {
  pairs(iris,
    upper.panel = function(x,y,..) panel.cor(x, y, n.adjust = ncol(iris) * (ncol(iris) - 1) / 2),
    lower.panel = points)
})
