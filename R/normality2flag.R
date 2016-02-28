normality2flag<-structure(
function # Sample normality estimate flag.
##<<details
## 'normality2flag' estimates the normality of a vector vector 'x' and
## returns a discrete flag assessing the normality (normal,
## non-normal, don't know).
##
## TODO: allow user-supplied normality test?
##
##<<seealso dagostinoTest, normality2color
(x ##<< a numeric vector to be tested for normality
) {
  if (length(na.omit(x))>=6) {
    # compute omnibus D'Agostino normality test P-value
    p<-dagostinoTest(x)[2,3]
    if (is.na(p)) rv<-3
    else if (p<.05) rv<-2
    else rv<-1
  } else {
    rv<-3
  }
  return(rv)
  ### a flag of value 1 if normality is not rejected at the 5% level,
  ### 2 if normality is rejected at the 5% level, and 3 if normality
  ### can't be assesed (not enough data etc.)
},ex=function() {
  normality2flag(rnorm(30))
  normality2flag(rnorm(3))
  normality2flag(runif(30))
})
