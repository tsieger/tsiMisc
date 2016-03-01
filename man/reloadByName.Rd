\name{reloadByName}
\alias{reloadByName}
\title{Reload R package(s).}
\description{This is a simple wrapper for
devtools::reload(devtools::inst(pkgName)).}
\usage{reloadByName(pkgName = "tsiMisc")}
\arguments{
  \item{pkgName}{name (or a vector of names) of R packages to be reloaded}
}



\author{Tomas Sieger}




\examples{
library(MASS)
reloadByName('MASS')
}
