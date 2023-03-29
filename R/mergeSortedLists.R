mergeSortedLists<-structure(
function # Merge sorted numeric lists of almost increasing values.
##description<<
## Given a list of numeric vectors, in each of which there come almost
## increasing sequences (that can be interrupted by decreasing values),
## \code{mergeSortedLists} merges them such that
## i) the relative order of any two values from an individual vector is
## preserved in the result, and
## ii) the resulting vector is sorted in increasing order, if this would
## not invalidate the first condition.
## In case of ties, the value from former vector takes precedence.
## For example: the list \code{list(c(1,3,5,3), c(2,4,1,2))} gives the
## result of \code{c(1,2,3,4,1,2,5,3)}.
##
(x, ##<< a list of numeric vectors
dbg = 0 ##<< debug level
) {
  # get rid of empty vectors
  len<-sapply(x,length)
  if (length(len)==0) len<-c()
  if (dbg) .pn(len)
  x<-x[len>0]
  len<-len[len>0]

  # number of vectors
  n<-length(x)
  if (dbg) .pn(n)

  # indices (order) of values in x
  idx<-x
  cml<-0
  for (i in seq(along=x)) {
    idx[[i]]<-cml+seq(along=x[[i]])
    cml<-cml+length(x[[i]])
  }
  if (dbg) .pn(idx)

  # pointer (current position) in each vector
  pos<-rep(1,n)

  # Given a list of vectors, extract values from given positions in
  # each vector.
  # x = list of numeric vectors
  valAtPos<-function(x,pos) {
    stopifnot(length(x)==length(pos))
    rv<-numeric(length(x))
    for (i in seq(along=x)) rv[i]<-x[[i]][pos[i]]
    return(rv)
  }

  # order of first values (heads) in individiual vectors
  o<-rank(valAtPos(x,pos),ties='first')
  if (dbg) .pn(o)

  # resulting vector and indices of samples
  v<-numeric(n)
  vidx<-numeric(n)
  # index in the result vector
  vi<-1
  # number of samples to process
  m<-sum(len)
  while (m>0) {
    if (dbg) catnl(m,'samples to process')
    o1<-o[1]
    if (dbg) catnl('taking value',x[[o1]][pos[o1]],'from vector',o1)
    v[vi]<-x[[o1]][pos[o1]]
    vidx[vi]<-idx[[o1]][pos[o1]]
    vi<-vi+1
    # advance the pointer into the o1-th vector
    pos[o1]<-pos[o1]+1
    m<-m-1
    if (pos[o1]>len[o1]) {
      # reached the end of vector o1,
      # remove this vector from consideration
      if (dbg) catnl(' vector',o1,'exhausted')
      x<-x[-o1]
      idx<-idx[-o1]
      pos<-pos[-o1]
      if (dbg) catnl('o before:',paste(o))
      o<-o[-1]
      o[o>o1]<-o[o>o1]-1
      if (dbg) catnl('o  after:',paste(o))
      n<-n-1
    } else {
      # sort the next value from x[[o1]] into the heads
      nextVal<-x[[o1]][pos[o1]]
      if (dbg) catnl(' sorting',nextVal,'into heads',valAtPos(x,pos)[-1])
      i<-2
      while (i<=n) {
        if (nextVal < x[[o[i]]][pos[o[i]]] ||
          nextVal==x[[o[i]]][pos[o[i]]] && o1<i) break
        i<-i+1
      }
      if (dbg) catnl('  inserting at pos',i)
      o<-c(o[rseq(2,i-1,1)],o1,o[rseq(i,n,1)])
      if (dbg) catnl('  o:',paste(o))
      if (dbg) catnl('  heads:',paste(valAtPos(x,pos)))
      if (dbg) catnl('  sorted heads:',paste(valAtPos(x,pos)[o]))
    }
  }

  return(list(x=v,idx=vidx))
  ### A list of two numeric vectors:
  ### \code{x}, the resulting vector of merged values, and
  ### \code{idx}, the indices of the selected values.
},ex=function() {
  mergeSortedLists(list(c(1,3,5,3), c(2,4,1,2)),dbg=1)
  mergeSortedLists(list(1:10))
  mergeSortedLists(list(1:2,1:2))
  mergeSortedLists(list(c(),1,c()))
})
