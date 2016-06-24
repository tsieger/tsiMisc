rseq<-structure(
function # Robust sequence generation resembling the matlab ':' operator.
##description<<
## \code{rseq} is similar to \code{\link[base]{seq}} with the only
## difference in the case when \code{by} is specified and goes in the
## opposite direction to \code{from} to \code{to}. In this case
## \code{\link[base]{seq}} raises an error, while \code{rseq} returns
## an empty sequence.
(from = 1, ##<< from
to = 1, ##<< to
by = NULL, ##<< increment
... ##<<
) {
  if (!missing(by)) {
    if (from==to) {
      rv<-from
    } else if ((from<to)!=(by>0)) {
      rv<-vector(class(from),0)
    } else {
      rv<-seq(from=from,to=to,by=by,...)
    }
  } else {
    rv<-seq(from=from,to=to,...)
  }
  return(rv)
  ### Sequence similar to the result of \code{\link[base]{seq}}.
},ex=function() {
  rseq(from = 1, to = 0, by = 1)
})
