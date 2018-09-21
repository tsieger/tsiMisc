\name{showObjectsSize}
\alias{showObjectsSize}
\alias{\code{.sos}}

\title{R object size dump.}
\description{\code{showObjectsSize} shows how much memory do objects in specific
environment(s) occupy. It builds upon \code{\link{object.size}} and
gathers information about the size and mode of objects in specific
environment(s). It supports environment name filtering using regular
expressions, object mode filtering, as well as filtering by size.
}
\usage{showObjectsSize(mode = NULL, n = 20, env = parent.frame(), recursive = FALSE, 
    envNameRegexpr = NULL, all.names = TRUE, sizeUnits = 1024 * 
        1024, verbose = FALSE)}
\arguments{
  \item{mode}{mode of objects to show (see 'mode'), use code{NULL} to
show objects of all modes.}
  \item{n}{number of objects to show (the first \code{n} biggest objects
will be shown).}
  \item{env}{environment to operate in.}
  \item{recursive}{shall we iteratively look in parent environments?}
  \item{envNameRegexpr}{regular expression character filter applied
to the names of environments. If not \code{NULL}, only environments
matched by \code{envNameRegexpr} are considered.}
  \item{all.names}{if \code{TRUE}, all object names are returned.
If \code{FALSE}, names which begin with a '.' are omitted.}
  \item{sizeUnits}{the units of size (defaults to 1MB)}
  \item{verbose}{if \code{TRUE}, progress is reported.}
}
\details{Partially based on
\url{http://jeromyanglim.blogspot.cz/2009/11/memory-management-in-r-few-tips-and.html}.
}
\value{A data frame of columns \code{name}, \code{size}, \code{mode},
and \code{env}.
In addition, the total size of all objects found is put into
the \code{totalSize} attribute of the data frame.}

\author{Tomas Sieger <tomas.sieger@seznam.cz> [aut, cre]}



\seealso{\code{\link{object.size}}, \code{\link{ls}}, \code{\link{mode}}}
\examples{
showObjectsSize(n=20)

if (require('MASS')) {
  showObjectsSize(mode = 'numeric', envNameRegexpr = glob2rx('package:MASS'), rec = TRUE)
}
}
