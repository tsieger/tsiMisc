\name{blindFriendlyColors}
\alias{blindFriendlyColors}
\title{Blind friendly colors.}
\description{Returns RGB values of blind-friendly colors.
}
\usage{blindFriendlyColors(names = TRUE)}
\arguments{
  \item{names}{if TRUE, the returned vector will be named}
}
\details{Blind-friendly colors were taken from the Nature Methods Point of
view (see the reference below). The colors form a qualitative palette.
}
\value{a vector holding text \code{rgb} values of 8 blind-friendly colors.}
\references{Wong, Bang: Points of view: Color blindness, Nature
Methods 8 (6) June 2011, 10.1038/nmeth.1618.

Chromatic vision simulator showing how people suffering from
protanopia, deuteranopia and tritanopia perceive colors:
http://asada.tukusi.ne.jp/webCVS/}
\author{Tomas Sieger}




\examples{
# list color names
print(names(bfc()))

# direct use of a specific color
print(bfc()["Orange"])

# show all colors
x<-bfc()
tmp<-rev(seq(along=names(x)))
  # needed to let pkgdonw find graphics::plot():
  require(graphics)
plot(c(.8,1.3),c(0,length(x)+1),ty='n',frame=FALSE,xaxt='n',yaxt='n',xlab='',ylab='')
points(rep(1,length(x)),tmp,pch=19,cex=6,col=x)
text(rep(1.05,length(x)),tmp,names(x),adj=0)
}
