pvalStars<-structure(
function # p-value text summary.
##description<<
## 'pvalStars' creates a graphical representation of p-value(s) (AKA
## "significance stars").
(p, ##<< p-value(s)
width=NULL, ##<< width of text summary (number of characters), if not
## \code{NULL}, the summary gets padded with spaces to consist
## of at least \code{width} characters
right=TRUE ##<< the type of padding
) {
  # prepend '1' to p-value(s) to make it working even for a single NA
    rv<-symnum(c(1,p), corr = FALSE, na = '?',
      cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
      symbols = c("***", "**", "*", ".", " "))
  # remove the first element related to the prepended 1
  rv<-rv[-1]
  # prepend spaces, if requested
  if (!is.null(width) && length(rv)>0) {
    for (i in 1:length(rv)) {
        if (nchar(rv[i])<width) {
            pad<-paste0(rep(' ',width-nchar(rv[i])),collapse='')
            if (right) {
                rv[i]<-paste0(pad,rv[i])
            } else {
                rv[i]<-paste0(rv[i],pad)
            }
        }
    }
  }
  return(rv)
  ### a vector of character strings representing the significance of
  ### 'p'
},ex=function() {
  p<-c(1,.1,.05,.01,.001,.000001,NA)
  cbind(p,pvalStars(p))
})
