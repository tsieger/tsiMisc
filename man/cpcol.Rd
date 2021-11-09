\name{cpcol}
\alias{cpcol}
\title{Copy columns in a data frame.}
\description{\code{\link{cpcol}} copies columns in a data frame.
}
\usage{cpcol(d, on, nn, i = NULL, 
    regexp = FALSE, multi = FALSE)}
\arguments{
  \item{d}{data frame}
  \item{on}{original column name (or part of it, or a regexpr)}
  \item{nn}{desired column name (or part of it, or a regexpr)}
  \item{i}{optional column index that must match the column with the name
denoted by \code{on} (this serves as a sanity check ensuring the
change affects the intended column)}
  \item{regexp}{logical flag indicating whether \code{on} and
\code{nn} contain regular expressions}
  \item{multi}{logical flag indicating whether to change multiple
coumns at once}
}



\author{Tomas Sieger}



\seealso{rencol}
\examples{
tmp<-data.frame(a=1:2,b=2:3)
cpcol(tmp,'a','a2')
# copy with explicit column idx check
cpcol(tmp,'a','a2',1)

# copy all columns starting with 'a.' as 'b.'
tmp<-data.frame(a.x=1:2,a.b=2:3)
cpcol(tmp,'a.','b.',multi=TRUE)
}
