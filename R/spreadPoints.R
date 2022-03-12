spreadPoints<-structure(
function # Spread points in 2D vertically to eliminate their overlap.
##description<<
## Starting from points with low Y values, shift points lying closely
## above them a little bit further up in attempt to eliminate their
## vertical overlap. This is useful e.g. when placing text annotation
## over a graph with a need to avoid annotation overlap.
(
xy, ##<< xy coordinates of points
xoverlap, ##<< span of neighbourhood in the X coordinate; points closer to
## \code{xoverlap} to each other will be considered overlapping
yoverlap, ##<< span of neighbourhood in the Y coordinate; points closer to
## \code{yoverlap} to each other will be considered overlapping
dbg = 0 ##<< debug level
) {
  stopifnot(ncol(xy)==2)

  if (dbg) catnl('xoverlap:',xoverlap)
  if (dbg) catnl('yoverlap:',yoverlap)

  vertOffsets<-numeric(nrow(xy))
  if (nrow(xy)>1) {
    # order points by increasing Y value
    ord<-order(xy[,2],decreasing=FALSE)
    for (oi in 2:length(ord)) {
      o<-ord[oi]
      # find X-close points
      cndX<-abs(xy[o,1]-xy[ord[1:(oi-1)],1])<xoverlap
      if (dbg) if (sum(cndX)>0) catnl(ord[1:(oi-1)][cndX],'close to',o)
      if (dbg) if (sum(!cndX)>0) catnl(ord[1:(oi-1)][!cndX],'far from to',o)
      if (sum(cndX)>0) {
        # distance from the current point to the nearest point lying above it
        df<-min(xy[o,2]-xy[ord[1:(oi-1)],2][cndX])
        if (dbg) catnl('df:',df)
        if (df<yoverlap) {
          if (dbg) catnl('shifting',o,'by',yoverlap-df,'from',xy[o,2],'to',xy[o,2]+yoverlap-df)
          xy[o,2]<-xy[o,2]+yoverlap-df
          vertOffsets[o]<-yoverlap-df
        }
      }
    }
  }
  attr(xy,'vertOffsets')<-vertOffsets
  return(xy)
  ### Updated \code{xy} having overlaps resolved. In addition, there is
  ### 'vertOffsets' attribute holding the offsets added to individual
  ## points.
},ex=function() {
  xy<-rbind(
    c(4,0),
    c(4,.1),
    c(7,1),
    c(7,2),
    c(8,2.1))
   labels<-paste('label',1:nrow(xy))
   xy2<-spreadPoints(xy,xoverlap=2,yoverlap=1)
   layout(rbind(1:2))
   plot(xy,ty='n',main=c('original positions','(overlapping)'),frame=FALSE,xlim=c(0,10),ylim=c(0,10))
   text(xy,labels)
   plot(xy2,ty='n',main=c('spread positions','(non-overlapping)'),frame=FALSE,xlim=c(0,10),ylim=c(0,10))
   text(xy2,labels)
   layout(1)
})
