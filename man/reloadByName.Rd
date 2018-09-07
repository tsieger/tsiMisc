\name{reloadByName}
\alias{reloadByName}
\title{Reload R package(s).}
\description{This is a simple wrapper for
\code{\link[devtools]{reload(devtools::inst(pkgName))}}.}
\usage{reloadByName(pkgName = "tsiMisc")}
\arguments{
  \item{pkgName}{name (or a vector of names) of R package(s) to be reloaded}
}



\author{Tomas Sieger <tomas.sieger@seznam.cz> [aut, cre]}




\examples{
library(MASS)
reloadByName('MASS')
# or without quotes as
reloadByName(MASS)
}
