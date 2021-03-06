\name{reloadByName}
\alias{reloadByName}
\title{Reload R package(s).}
\description{This is a simple wrapper for
\code{\link[devtools]{reload}(\link[pkgload]{inst}(\code{pkgName})}}
\usage{reloadByName(pkgName = "tsiMisc")}
\arguments{
  \item{pkgName}{name (or a vector of names) of R package(s) to be reloaded}
}



\author{Tomas Sieger}




\examples{
library(MASS)
reloadByName('MASS')
# or without quotes as
reloadByName(MASS)
}
