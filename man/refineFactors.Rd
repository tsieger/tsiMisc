\name{refineFactors}
\alias{refineFactors}
\title{Refine factors in two twin data frames.}
\description{Refine two data frames 'x1' and 'x2' to be usable as a pair of
a train/test set pair in a modeling or classification task, such
that a model/classifier (e.g. a binomial GLM) can be trained on the
train set and can be then directly applied to the test set.
Note that had the sets not been refined this way, a model trained on
a train set could not have been directly applied to the test set
because a new factor level (not appearing in the train set) would
have appeared in it, giving no clue how to predict response using
such an unknown factor level.
}
\usage{refineFactors(x1, x2, unify = TRUE, dropSingular = TRUE, naLimit = Inf, 
    k = 5, naLevelName = "(NA)", verbose = FALSE, debug = FALSE)}
\arguments{
  \item{x1}{first data frame}
  \item{x2}{second data frame}
  \item{unify}{shall factors be unified?}
  \item{dropSingular}{shall factors having only a single level be
removed?}
  \item{naLimit}{numeric columns containing more than 'naLimit' NA's
(in 'x1' and in 'x2') will be converted into a factor created by
'cut'-ting the numeric values into 'k' intervals, and adding a
special 'naLevelName' level to hold the missing values}
  \item{k}{the number of intervals into which numeric columns having
at least 'naLimit' NA's (in 'x1' and in 'x2') will be converted}
  \item{naLevelName}{the name of the special factor level
used to represent missing values}
  \item{verbose}{report progress?}
  \item{debug}{if TRUE, debugs will be printed. If numeric of value
greater than 1, verbose debugs will be produced.}
}
\details{Usually, the refinement consists of i) making the levels of factors
in individual columns in each of the sets identical, and
ii) removing columns containing factors of only a single level. This
first task is achieved by removing rows in which appears a factor of
level not appearing in the twin data frame, and dropping unused
levels from factors.
}
\value{A list of refined 'x1' and 'x2',}

\author{Tomas Sieger <tomas.sieger@seznam.cz> [aut, cre]}



\seealso{'dropLevels'}
\examples{
# unify factor levels and remove constant factors:
x<-data.frame(x = 1:6, y = c('a','b','c','b','c','d'), z = c('d','c','c','c','c','d'))
x.train <- x[1:3, ]
x.test <- x[4:6, ]
print(x.train)
print(x.test)
refineFactors(x.train, x.test)
# Note: 'x[1,]' and 'x2[3,]' dropped because it had no counterpart
#   in the twin data frame.
# Note: 'x$z' dropped because after removal of 'x[1,]' and 'x2[3,]',
#   there was only a single factor level left, which was dropped, by default.

# unify factor levels but keep constant factors:
refineFactors(x.train, x.test, dropSingular = FALSE)
# Note: now 'x$z' is left

# convert numeric columns with many NA's into a factor
x<-data.frame(x = 1:10, y = c(NA,NA,1,2,3,NaN,NA,1,2,3), z = c(1,2,NA,4,5,1,2,NA,4,5))
x1 <- x[1:5, ]
x2 <- x[6:10, ]
print(x1)
print(x2)
refineFactors(x1, x2, naLimit=2)

# add a special 'NA' level to factors with many NA's
x<-data.frame(x = 1:10, y = factor(c(NA,NA,1,2,3,NA,NA,1,2,3)), z = factor(c(1,2,NA,4,5,1,2,NA,4,5)))
x1 <- x[1:5, ]
x2 <- x[6:10, ]
print(x1)
print(x2)
refineFactors(x1, x2, naLimit=2)

# NaN in factors differs from NA - this would behave differently:
# x<-data.frame(a = 1:10, b = factor(c(NA,NA,1,2,3,NA,NaN,1,2,3)), c=factor(c(1,2,NA,4,5,1,2,NA,4,5)))

}
