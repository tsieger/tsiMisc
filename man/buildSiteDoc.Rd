\name{buildSiteDoc}
\alias{buildSiteDoc}
\title{build_site build}
\description{**OBSOLETED**
This is a simple delegate wrapper for
\code{staticdocs::build_site} to generate package doc for an
R package. In addition, if there are any rgl devices opened by
\code{\link[staticdocs]{build_site}}, they get closed.

TODO: Remove this rgl-closing hack.}
\usage{buildSiteDoc(path = "~/src/Rweb/tsiMisc")}
\arguments{
  \item{path}{\code{path} to an R package source}
}



\author{Tomas Sieger}




\examples{
##dontrun<<
# buildSiteDoc('~/src/R/myPackage')
##end<<
}
