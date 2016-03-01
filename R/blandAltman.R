blandAltman<-structure(
function # Bland-Altman plot.
##description<<
## The Bland-Altman plot (AKA Tukey mean-difference plot) is a
## graphical tool to measure agreement between two estimates of a
## single variables methods.
##
##details<<
## Code taken from http://rstats.tiddlyspot.com/#Bland-Altman and
## updated slightly.
##
##references<< Bland JM, Altman DG (1986) _Statistical methods for
## assessing agreement between two methods of clinical measurement.__
## Lancet 327 (8476): 30710.
(x1, ##<< the first estimate
x2, ##<< the second estimate
xlab = "Average", ##<< a label of x-axis on which the mean of the two
## estimates is plotted
ylab = "Difference", ##<< a label of y-axis on which the difference
## between the two estimates is plotted
alpha = 0.05 ##<< significance level of the confidence interval
) {
  means <- (x1+x2)/2
	diffs <- x1 - x2
  qnt<-qnorm(1-alpha/2)

	plot(diffs ~ means,pch=16,
    xlab=xlab,ylab=ylab,
    ylim=c(min(c(diffs,mean(diffs)-qnt*sd(diffs))),max(c(diffs,mean(diffs)+qnt*sd(diffs)))))
	abline(h=mean(diffs)-c(-qnt,0,qnt)*sd(diffs),lty=2)

  se<-sqrt(var(diffs)/length(diffs))
  se_limit <- sqrt(qnt*var(diffs)/length(diffs))
  u<-mean(diffs) + 2*sd(diffs) + c(-1,1) * qt(1-alpha/2, df=length(diffs)-1) * se_limit
  l<-mean(diffs) - 2*sd(diffs) + c(-1,1) * qt(1-alpha/2, df=length(diffs)-1) * se_limit

  return(list(mean=means,diff=diffs,se=se,lower=l,upper=u))
  ### a list of 'mean' (the of the two estimates), 'diff' (the diff of
  ### the estimates, 'se' (the standard error of the differences), and
  ### the '1-alpha/2' confidence intervals for the 'mean +/- 2 * SD of
  ### diffs' given as 'lower' and 'upper'
},ex=function() {
  n<-20
  x<-rnorm(n)
  x1<-x+rnorm(n,0,.1)
  x2<-x+rnorm(n,0,.1)
  blandAltman(x1,x2)
})
