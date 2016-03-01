pvalStars<-structure(
function # p-value text summary.
##description<<
## 'pvalStars' creates a graphical representation of p-value(s) (AKA
## "significance stars").
(p ##<< p-value(s)
) {
  # prepend '1' to p-value(s) to make it working even for a single NA
    rv<-symnum(c(1,p), corr = FALSE, na = '?',
      cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
      symbols = c("***", "**", "*", ".", " "))
  # remove the first element related to the prepended 1
  return(rv[-1])
  ### a vector of character strings representing the significance of
  ### 'p'
},ex=function() {
  p<-c(1,.1,.05,.01,.001,.000001,NA)
  cbind(p,pvalStars(p))
})
