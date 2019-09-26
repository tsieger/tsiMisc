mixcol <- structure(
function # Mix colors in textual representation.
##description<<
##
## Mix (blend) two colors specified as text. This is high-level function
## based on the \code{\link[colorspace]{mixcolor}}.
##
##seealso<< \code{\link[colorspace]{mixcolor}},
## \code{\link[grDevices]{col2rgb}},
## \code{\link[colorspace]{RGB}},
## \code{\link{rgb2hex}}.
(col1, ##<< the first color (a character vector or an integer color code)
col2, ##<< the second color (a character vector or an integer color code)
alpha = 0.5 ##<< the mixed color is obtained by combining an amount
## \code{1-alpha} of \code{col1} with an amount \code{alpha} of \code{col2}.
){
  if (!is.numeric(alpha) || any(alpha<0) || any(alpha>1)) {
    stop('invalid "alpha" argument')
  }
  if (!is.character(col1) && !is.integer(col1)) {
    stop('invalid "col1" color specification, expected a character vector or an integer')
  }
  if (!is.character(col2) && !is.integer(col2)) {
    stop('invalid "col2" color specification, expected a character vector or an integer')
  }

  col1<-RGB(t(col2rgb(col1)))
  col2<-RGB(t(col2rgb(col2)))
  mc<-mixcolor(alpha,col1,col2)
  col<-rgb2hex(mc)

  return(col)
  ### textual representation of the mixed color
},ex=function() {
  col1<-mixcol('yellow','black',0)
  col2<-mixcol('yellow','black', .25)
  col3<-mixcol('yellow','black', .5)
  col4<-mixcol('yellow','black', .75)
  col5<-mixcol('yellow','black',1)
  plot(0:4,rep(0,5),xlim=c(-.5,5.5),ty='p',pch=21,cex=15,col='black',
    bg=c(col1,col2,col3,col4,col5),frame=FALSE,xlab='',ylab='',xaxt='n',yaxt='n')
})
