compensatePValue<-structure(
function # P-value compensation for multiple comparisons.
##description<<
## **OBSOLETED**
## \code{\link{compensatePValue}} compensates a p-value for the number of tests
## performed.
##
##references<<
## SISA, \url{http://www.quantitativeskills.com/sisa/calculations/bonhlp.htm}
## Sture Holm, A Simple Sequentially Rejective Multiple Test Procedure, Scand J Statist 6: 65-70, 1979
##note<< deprecated, not working properly, use \code{\link[stats]{p.adjust}} instead
##
##seealso<< \code{\link[stats]{p.adjust}}, \code{\link{compensateAlpha}}
(p, ##<< p-value(s) to be compensated
n=length(p), ##<< number of tests made
method = c('bonferroni', 'sidak', 'holm-bonferroni'), ##<< compensation method,
## 'bonferroni' is conservative, does not assume independence
## of the tests, and controls for the family-wise error (FWE),
## 'sidak' is more liberal, and controls for FWE only under the
## assumption of independence of the tests performed,
## 'holm-bonferroni' is an iterative procedure controlling for FWE
r = 0 ##<< optional mean "correlation between the tests made",
## the extreme value of 0 leads to full compensation, the extreme value
## of 1 leads to no compensation (as all the tests are considered equal).
## See the SISA help for explanation. Note that the FWE is controlled
## in the strict sense only for 'r=0'.
) {
  if (r<0 || r>1) stop('correlation must be between 0 and 1')
  n<-n^(1-r)

  method<-match.arg(method)
  if (method=='sidak') {
    p<-1-(1-p)^n
  } else if (method=='bonferroni') {
    p<-pmin(1,p*n)
  } else if (method=='holm-bonferroni') {
    # sorted p-values
    #  .01   .02   .04   .05
    # compared with .05/x, x:
    #   4     3     3     3
    #  .0125 .0067 .0067 .0067
    # rejected?
    #  y     n     n     n
    # p-values * (n-j+1):
    #  .04   .06   .08   .05
    # adjusted p-values (pc_i = max_{j<=i} min(1, (n-j+1)p_{(i)}), see
    # p.adjust and
    # https://en.wikipedia.org/wiki/Holm%E2%80%93Bonferroni_method#Adjusted_P-value
    #  .04   .06   .08   .08
    # (this adjustment is not implemented)
    stop('p-values are not adjusted properly, use \'stats::p.adjust\' instead')
    # TODO

    # sort p-values
    o<-order(p)
    ps<-p[o]
    # compute 'rnk' such that 'ps[rnk]' equals 'p'
    rnk<-vector('numeric',length(p))
    rnk[o]<-1:length(p)
    # candidate multiplicative factors
    ns<-seq(length(p),1,-1) # in case of rejecting H0, we keep decreasing
    # do we reject each p-value?
    rejected<-ps<.05/ns
    # as we shall compare not to teh decreasing values of 'ns', but
    # shall stop decreasing after first accepted H0, we need to correct
    # for this - take only the starting contiguous segment of TRUEs
    rejected<-as.logical(cumprod(rejected))
    # final multiplicative factors - we keep decreasing values while we
    # keep rejecting H0, and constant values afterwards
    if (sum(rejected)<length(rejected)) {
      # index of the last rejected H0
      k<-sum(rejected)
      ns[(k+1):length(rejected)]<-ns[k+1]
    }
    # finally compensate the p-values
    p<-pmin(1,p*ns[rnk])
  } else if (method=='FDR-BH') {
    # sorted p-values
    #  .01   .02   .04   .049
    # compared with .05/x, x:
    #   4     3     2     1
    #  .0125 .0067 .025 .05
    # rejected?
    #  y     n     n     y
    # maximal index for which H0 is rejected: 4
    # corrected (sorted) p-values - HOW TO COMPUTE?
    stop('unimplemented')
  } else stop('unsupported method "',method,'"')
  return(p)
  ### Compensated p-value. It equals \code{min(1,p*n)} for the \code{'bonferroni'}
  ### method, \code{1-(1-p)^n} for the \code{'sidak'} method, and the result of the
  ### iterative procedure for the method of \code{'holm-bonferroni'}.
},ex=function() {
  # demonstrate the difference between Bonferroni and Sidak:
  compensatePValue(.025,2,method='bonferroni')
  compensatePValue(.025,2,method='sidak')

  # demonstrate the iterative Holm-Bonferroni method:
  #compensatePValue(c(.01,.02,.04,.05),method='holm-bonferroni')
  #compensatePValue(c(.05,.04,.01,.02),method='holm-bonferroni')
  # use 'stats::p.adjust' instead
})
