columnIndexOf<-structure(
function # Indices of data frame columns identified by name.
##description<<
## Indices of data frame columns identified by name.
(x, ##<< name(s) of columns to be searched for
d, ##<< data frame whose columns shall be searched
regexp = TRUE ##<< If TRUE, \code{x} of character type are interpreted as
## regular expressions.
) {
  return(indexOf(x,colnames(d),regexp))
  ### indices of requested columns
},ex=function() {
  # Find indices of 'Petal.Length' and 'Sepal.Length' columns in the
  # 'iris' data:
  columnIndexOf(c('Sepal.Length','Petal.Length'),iris)

  # Alternatively, using regular expressions:
  columnIndexOf(c('.*Length'),iris)
})
