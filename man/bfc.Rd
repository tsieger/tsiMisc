\name{bfc}
\alias{bfc}
\title{Blind friendly colors.}
\description{Returns RGB values of blind-friendly colors.
}
\usage{bfc()}
\details{Blind-friendly colors were taken from the Nature Methods Point of
view (see the reference below). The colors form a qualitative palette.
}
\value{a named list holding text 'rgb' values of 8 blind-friendly colors.}
\references{Wong, Bang: Points of view: Color blindness, Nature
Methods 8 (6) June 2011, 10.1038/nmeth.1618.

Chromatic vision simulator showing how people suffering from
protanopia, deuteranopia and tritanopia perceive colors:
http://asada.tukusi.ne.jp/webCVS/}
\author{Tomas Sieger <tomas.sieger@seznam.cz> [aut, cre]}




\examples{
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
}
