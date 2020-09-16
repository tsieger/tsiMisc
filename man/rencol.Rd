\name{rencol}
\alias{rencol}
\title{Rename columns in a data frame.}
\description{\code{\link{rencol}} renames columns in a data frame.
}
\usage{rencol(d, on, nn, i = NULL, 
    multi = FALSE)}
\arguments{
  \item{d}{data frame}
  \item{on}{original column name (or a regexpr)}
  \item{nn}{desired column name (or a regexpr)}
  \item{i}{optional column index that must match the column with the name
denoted by \code{on} (this serves as a sanity check ensuring the
change affects the intended column)}
  \item{multi}{logical flag indicating whether to change multiple
coumns at once}
}



\author{Tomas Sieger}




\examples{
tmp<-data.frame(a=1:2,b=2:3)
rencol(tmp,'a','a2')
rencol(tmp,'a','a2',1)

# get rid of a common prefix
tmp<-data.frame(prefix.a=1:2,prefix.b=2:3)
rencol(tmp,'prefix.','',multi=TRUE)
}
