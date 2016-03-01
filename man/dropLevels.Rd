\name{dropLevels}
\alias{dropLevels}
\title{Drop unused levels in a factor.}
\description{This is only a wrapper for 'gdata::drop.levels'.}
\usage{dropLevels(x, reorder = TRUE, ...)}
\arguments{
  \item{x}{object to be processed}
  \item{reorder}{should factor levels be reordered using 'reorder.factor'?}
  \item{\dots}{additional arguments to 'reorder.factor'}
}

\value{Input object without unused levels.}

\author{Tomas Sieger}



\seealso{'gdata::drop.levels'}
\examples{
f<-factor(c(NA,1,1,2),levels=c(0,1,2),labels=c('a','b','NA'))
dropLevels(f)
}
