\name{countNa}
\alias{countNa}
\title{Count NA's in a data frame.}
\description{'countNa' computes the number of missing values in a data frame.
It counts the number of missings in each column, the number of rows
in which a value in at least one columns is missing, and the
expected number of rows with at least one missing value (computed
under the assumption of independence of missingness in individual
columns). Number of rows left are also given.
Optionally, combinations of columns reaching highest joint
missingness is also reported (if 'combColCount > 1' and the
number of columns in 'x' is at least 2).}
\usage{countNa(d, sort = TRUE, decreasing = FALSE, combColCount = 3)}
\arguments{
  \item{d}{a data frame}
  \item{sort}{sort columns of 'x' by the number of missings?}
  \item{decreasing}{if sorting by the number of missing, should
the sort be decreasing or increasing?}
  \item{combColCount}{maximum number of columns to combine when finding a
combination of columns reaching the highest number of missings}
}

\value{A data frame (or a list of two data frames) describing the
missingness. The first data frame consists of rows describes the
missingness in individual columns, plus the missingness in the
combination of all columns (in a row called 'any'), plus the
average missingness (in a row called 'average').
The second data frame (if requested) describes the combinations
of at most 'combColCount' columns of 'x' reaching highest joint
missingness.}

\author{Tomas Sieger <tomas.sieger@seznam.cz> [aut, cre]}




\examples{
d<-data.frame(x1=1,x2=2,x3=1:4,y=c(1,NA,2,NA),z=c(NaN,NaN,3,4))
d
countNa(d)
}
