bfc<-blindFriendlyColors<-structure(
function # Blind friendly colors.
##description<<
## Returns RGB values of blind-friendly colors.
##
##details<<
## Blind-friendly colors were taken from the Nature Methods Point of
## view (see the reference below). The colors form a qualitative palette.
##
##references<< Wong, Bang: Points of view: Color blindness, Nature
## Methods 8 (6) June 2011, 10.1038/nmeth.1618.
(
) {
  x<-data.frame(rbind(
    c('Black',            c(0, 0, 0)),
    c('Orange',           c(230, 159, 0)),
    c('Sky blue',         c(86, 180, 233)),
    c('Bluish green',     c(0, 158, 115)),
    c('Yellow',           c(240, 228, 66)),
    c('Blue',             c(0, 114, 178)),
    c('Vermillion',       c(213, 94, 0)),
    c('Reddish purple',   c(204, 121, 167))))
  colnames(x)<-c('name','r','g','b')
  y<-c()
  for (i in 1:nrow(x)) {
    tmp<-as.character(rgb(
      as.numeric(as.character(x[i,2]))/255,
      as.numeric(as.character(x[i,3]))/255,
      as.numeric(as.character(x[i,4]))/255))
    y<-c(y,tmp)
  }
  names(y)<-x[,1]
  return(y)
  ### a named list holding text 'rgb' values of 8 blind-friendly colors.
},ex=function() {
  # list color names
  print(names(bfc()))

  # direct use of a specific color
  print(bfc()["Orange"])

  # show all colors
  x<-bfc()
  tmp<-rev(seq(along=names(x)))
  plot(c(.8,1.3),c(0,length(x)+1),ty='n',frame=FALSE,xaxt='n',yaxt='n',xlab='',ylab='')
  points(rep(1,length(x)),tmp,pch=19,cex=6,col=x)
  text(rep(1.05,length(x)),tmp,names(x),adj=0)
})
