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

  x2<-x
  # we must convert to data.frame, as columns of a character matrix can't be
  # converted to numeric column by column
  if (!is.data.frame(x2)) x2<-as.data.frame(x2)

  if (ncol(x2)>0) {
    for (i in 1:ncol(x2)) {
      if (is.factor(x2[,i])) {
        x2[,i]<-as.numeric(x2[,i])
      } else if (is.character(x2[,i])) {
        # try to convert numbers in text form to numeric
        tmp<-tryCatch(as.numeric(x2[,i]),warning=function(e)NULL)
        if (!is.null(tmp)) {
          x2[,i]<-tmp
        } else {
          # maybe, the problem was with NA's, let's test that
          idxNonNA<-x2[,i]!='NA'
          tmp2<-tryCatch(as.numeric(x2[idxNonNA,i]),warning=function(e)NULL)
          if (!is.null(tmp2)) {
            x2[,i]<-tmp
          } else {
            x2[,i]<-as.numeric(as.factor(x2[,i]))
          }
        }
      } else {
        x2[,i]<-as.numeric(x2[,i])
      }
    }
  }

  if (is.matrix(x)) x2<-as.matrix(x2)
  return(x2)

},ex=function() {
  x<-data.frame(a=1:3,b=factor(c('a1','a2','a2')))
  dfc2num(x)

  x<-data.frame(a=1:3,b=c('1','2','2'))
  dfc2num(x)

  x<-cbind(matrix(1:4,2),c('a','b'))
  dfc2num(x)

  x<-cbind(c(1,2,NA,Inf),c('a','b','d','c'))
  dfc2num(x)

  x<-data.frame(num=c(1,2,NA,Inf),text=c('a','b','d','c'))
  dfc2num(x)
})
