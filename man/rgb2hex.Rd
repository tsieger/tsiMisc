\name{rgb2hex}
\alias{rgb2hex}
\title{Convert RGB color to text hexadecimal representation.}
\description{Convert an RGB color to '#RRGGBB' representation.
Useful e.g. to use a mixed color (as mixed by the \code{\link[colorspace]{mixcolor}}
function) as a color in calls to raw graphics function. See examples.

see https://gist.github.com/mbannert/e9fcfa86de3b06068c83}
\usage{rgb2hex(r, g = NULL, b = NULL)}
\arguments{
  \item{r}{red color component, or a column 3-element vector of R,G,B values, or an object of class 'RGB'}
  \item{g}{optional green color component}
  \item{b}{optional blue color component}
}



\author{Tomas Sieger <tomas.sieger@seznam.cz> [aut, cre]}




\examples{
require(colorspace)

col1<-RGB(t(col2rgb('yellow')))
col2<-RGB(t(col2rgb('black')))
mixCol<-mixcolor(.3,col1,col2)
col<-rgb2hex(mixCol)
plot(c(0,1,2),c(0,0,0),xlim=c(-.5,2.5),ty='p',pch=21,cex=15,col='black',
  bg=c('yellow',col,'black'),frame=FALSE,xlab='',ylab='',xaxt='n',yaxt='n')
}
