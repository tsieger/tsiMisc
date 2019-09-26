\name{mixcol}
\alias{mixcol}
\title{Mix colors in textual representation.}
\description{Mix (blend) two colors specified as text. This is high-level function
based on the \code{\link[colorspace]{mixcolor}}.
}
\usage{mixcol(r = 0.5, col1, col2)}
\arguments{
  \item{r}{amount of the first color (a number between 0 and 1)}
  \item{col1}{the first color}
  \item{col2}{the second color}
}

\value{textual representation of the mixed color}

\author{Tomas Sieger}



\seealso{\code{\link[colorspace]{mixcolor}},
\code{\link[grDevices]{col2rgb}},
\code{\link[colorspace]{RGB}},
\code{\link{rgb2hex}}.}
\examples{
col1<-mixcol(0,   'yellow','black')
col2<-mixcol( .25,'yellow','black')
col3<-mixcol( .5, 'yellow','black')
col4<-mixcol( .75,'yellow','black')
col5<-mixcol(1,   'yellow','black')
plot(0:4,rep(0,5),xlim=c(-.5,5.5),ty='p',pch=21,cex=15,col='black',
  bg=c(col1,col2,col3,col4,col5),frame=FALSE,xlab='',ylab='',xaxt='n',yaxt='n')
}
