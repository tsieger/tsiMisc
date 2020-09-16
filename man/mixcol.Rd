\name{mixcol}
\alias{mixcol}
\title{Mix colors in textual representation.}
\description{Mix (blend) two colors specified as text. This is high-level function
based on the \code{\link[colorspace]{mixcolor}}.
}
\usage{mixcol(col1, col2, alpha = 0.5)}
\arguments{
  \item{col1}{the first color (a character vector or an integer color code)}
  \item{col2}{the second color (a character vector or an integer color code)}
  \item{alpha}{the mixed color is obtained by combining an amount
\code{1-alpha} of \code{col1} with an amount \code{alpha} of \code{col2}.}
}

\value{textual representation of the mixed color}

\author{Tomas Sieger}



\seealso{\code{\link[colorspace]{mixcolor}},
\code{\link[grDevices]{col2rgb}},
\code{\link[colorspace]{RGB}},
\code{\link{rgb2hex}}.}
\examples{
col1<-mixcol('yellow','black',0)
col2<-mixcol('yellow','black', .25)
col3<-mixcol('yellow','black', .5)
col4<-mixcol('yellow','black', .75)
col5<-mixcol('yellow','black',1)
plot(0:4,rep(0,5),xlim=c(-.5,5.5),ty='p',pch=21,cex=15,col='black',
  bg=c(col1,col2,col3,col4,col5),frame=FALSE,xlab='',ylab='',xaxt='n',yaxt='n')
}
