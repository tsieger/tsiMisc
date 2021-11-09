plotLmPartRes<-structure(
function # Plot partial residuals of a simple linear model fit.
##description<<
## plotLmPartRes shows the relation between selected covariates from a lm model
## and the response, as well as the relation of those covariates and the response
## adjusted for the other covariates.
##
## The functionality of plotLmPartRes is very limited.
## Only numeric vectors are supported (factors, interactions, I()'s are NOT).
## \code{\link[car]{crPlot}} and \code{\link[car]{ceresPlot}} should be preferred.
##
##seealso<< \code{\link[car]{crPlot}}, \code{\link[car]{ceresPlot}}
(m, ##<< an object of class 'lm'
covNames=names(m$model)[-1] ##<< names of covariates to plot (defaults to all of them)
) {
  nCov<-length(covNames)
  opar<-par(mfcol=c(2,nCov))
  y<-m$model[,1] # TODO: extract response in a better way
  yName<-names(m$model)[1]
  #.pn(covNames)
  for (nm in covNames) {
    .pn(nm)
    x<-eval(parse(text=nm),m$model)
    #.pn(x)
    plot(x,y,xlab=nm,ylab=yName,main='unadjusted')
    lines(lowess(x,y),col='red')
    #
    tmp<-m$model
    #.pn(tmp)
    tmp[[nm]]<-0
    y2<-y-predict(m,newdata=tmp)
    plot(x,y2,xlab=nm,ylab=yName,main='adjusted for other covariates')
    lines(lowess(x,y2),col='red')
  }
  par(opar)
},ex=function() {
  # prepare data
  set.seed(1)
  n<-40
  h<-rnorm(n)
  f<-rnorm(n)
  w<-3*h-f+rnorm(n)
  d<-data.frame(h,w,f)
  # fit lm
  m<-lm(f~h+I(w-d),d)
  summary(m)
  # plot partial residuals
  plotLmPartRes(m)
})
