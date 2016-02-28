\name{plotCor}
\alias{plotCor}
\title{Plot decorated bivariate correlations.}

\usage{plotCor(x, method = c("pearson", "spearman", "spearmanExact", 
    "glmPoisson", "lm"), adjust = FALSE, i1 = NULL, i2 = NULL, 
    normalityColor = TRUE, aboveDiag = FALSE, silent = FALSE, 
    plot = TRUE, ...)}
\arguments{
  \item{x}{data frame}
  \item{method}{
}
  \item{adjust}{adjust p-values to the number of tests performed?
If TRUE, approximate adjustment is used, correcting for the
estimated effective number of tests performed, considering possible
multicollinearity between variables. The effective number of tests
is estimated as 'n ^ (1 - meanCorr)', where 'n' is the number of all
bivariate tests performed and 'meanCor' is the mean absolute value
of correlations between all pairs of variables considered. (For the
purpose of this estimation, Pearson correlation is used when
'method' is set to 'pearson' or 'lm', and Spearman correlation is
used otherwise.)
If FALSE (the default), no adjustment is performed.
If adjust='full' is used, adjustment to the full number of tests
performed is employed.
If 'adjust' is numeric, the number of tests performed is set to this
value.
If adjustment is requested, Bonferroni procedure is employed.}
  \item{i1}{optional vector of indices or names of column of 'x' that
form first elements of correlation pairs. By default, all columns
are considered.}
  \item{i2}{optional vector of indices or names of column of 'x' that
form second elements of correlation pairs. By default, all columns
are considered.}
  \item{normalityColor}{If TRUE, the diagonal histograms of
individual variables get colored according to the estimated
normality of the variables (normality in 'green', non-normality in
'red', indeterminate normality in 'gray').}
  \item{aboveDiag}{If TRUE, scatter plots will appear above the
diagonal and decorations below the diagonal. If FALSE (the default),
scatter plots will appear below the diagonal and decorations above
the diagonal.}
  \item{silent}{if TRUE, the number of tests compenstaing for gets
displayed}
  \item{plot}{if TRUE, a plot is produced.}
  \item{\dots}{further arguments passed on to 'pairs'}
}

\value{the estimated effective number of the tests performed}

\author{Tomas Sieger}




\examples{
#
plotCor(iris[,1:4])
plotCor(iris[,1:4],i1=c('Sepal.Length','Sepal.Width'),i2=c('Petal.Length','Petal.Width'))

# TODO: demonstrate more features
}
