strTrim<-structure(
function # Trim leading and trailing spaces from a string.
(x ##<< character string (or a vector of strings) to be trimmed
) {
  return(gsub("(^ +)|( +$)", "",x))
  ### trimmed string(s)
},ex=function() {
  strTrim(' Hello, World!  ')
  strTrim(c('a ', 'b ', ' c '))
})
