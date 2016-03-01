seqRob<-structure(
function # Robust sequence generation resembling the matlab ':' operator.
##description<<
## 'seqRob' is similar to 'base::seq' with the only difference in the
## case when 'by' is specified and goes in the opposite direction to
## 'from' to 'to'. In this case 'base::seq' raises an error, while
## 'seqRob' returns an empty sequence.
(from = 1, ##<< from
to = 1, ##<< to
by = NULL, ##<< increment
... ##<<
) {
  if (!missing(by) && (from<to)!=(by>0)) {
    rv<-vector(class(from),0)
  } else {
    if (!missing(by)) {
      rv<-seq(from=from,to=to,by=by,...)
    } else {
      rv<-seq(from=from,to=to,...)
    }
  }
  return(rv)
  ### Sequence similar to the result of 'base::seq'.
},ex=function() {
  seqRob(from = 1, to = 0, by = 1)
})
