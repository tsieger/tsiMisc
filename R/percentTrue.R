percentTrue<-structure(
function # Percentual ratio of TRUE cases.
##description<<
## `percent` computes the percentual ratio of TRUE cases
(x, ##<< a logical vector (or a vector that can be coerced to one)
digits = NULL ##<< number of decimal places (passed to 'round')
) {
  if (!is.logical(x)) {
    x<-as.logical(x)
  }
  rv<-100*sum(x)/length(x)
  if (!is.null(digits)) {
    rv<-round(rv,digits=digits)
  }
  return(rv)
  ### Percentual ratio of TRUE cases of 'x'.
},ex=function() {
  percentTrue(c(TRUE, TRUE, TRUE))
  percentTrue(c(TRUE, FALSE, FALSE), digits = 1)
  percentTrue(c(FALSE))
  percentTrue(NULL)
})
