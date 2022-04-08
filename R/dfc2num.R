dfc2num<-structure(
function # Convert columns to numeric.
##description<<
## \code{dfc2num} converts data frame (or matrix) columns to numeric type.
## Factors get converted to its numeric representation,
## textual representation of numbers get converted back to numbers,
## other texts get converted to factor and then to its numeric codes,
## and other types get converted using \code{as.numeric()}.
(x) {
  if (!is.matrix(x) && !is.data.frame(x)) stop('expected a matrix or a data.frame')
  if (ncol(x)>0) {
    for (i in 1:ncol(x)) {
      if (is.factor(x[,i])) {
        x[,i]<-as.numeric(x[,i])
      } else if (is.character(x[,i])) {
        # try to convert numbers in text form to numeric
        tmp<-tryCatch(as.numeric(x[,i]),error=function(e)NULL)
        if (!is.null(tmp)) {
          x[,i]<-tmp
        } else {
          x[,i]<-as.numeric(as.factor(x[,i]))
        }
      } else {
        x[,i]<-as.numeric(x[,i])
      }
    }
  }
  return(x)
},ex=function() {
  x<-data.frame(a=1:3,b=factor(c('a1','a2','a2')))
  dfc2num(x)

  x<-data.frame(a=1:3,b=c('1','2','2'))
  dfc2num(x)
})
