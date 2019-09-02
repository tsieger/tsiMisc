\name{perfInd}
\alias{perfInd}
\title{Compute performance indicators.}
\description{Compute several indicators to describe the performance of a binary
classifier, e.g. sensitivity, specificity, an estimate of the area
under the receiver operating characteristic, the Gini coefficient
etc. (see the return value).
}
\usage{perfInd(x, y = NULL, negativeFirst = TRUE, debug = FALSE)}
\arguments{
  \item{x}{a 2x2 classification table having predictions in rows, and
ground truth in columns (negative cases come first, by default, see
the \code{negativeFirst} argument), or a vector of predicted values
for each observation (coded, by default, such that negative cases
come first, e.g. as \code{0} and \code{1}, or \code{FALSE} and
\code{TRUE}, or as a factor). If a row (and a column) is missing,
a guess is made to make a 2x2 table from \code{x}. This requires
\code{x} to have rows/columns named as 'TRUE' and 'FALSE' such that
we would deduce which row/column is missing. See examples.}
  \item{y}{if \code{x} is a vector of predictions, \code{y} holds
the vector of ground truth classification for each observation
(coded, by default, such that negative cases come first, e.g. as
\code{0} and \code{1}, or \code{FALSE} and \code{TRUE}, or as a
factor).}
  \item{negativeFirst}{if TRUE, negative cases come first in both
rows and columns of \code{x}, or in vectors \code{x} and \code{y}}
  \item{debug}{if TRUE, debugs will be printed. If numeric of value
greater than 1, verbose debugs will be produced.}
}

\value{a list containing the following named entries:
\item{table}{classification table}
\item{tp}{true positives}
\item{tn}{true negatives}
\item{fn}{false negatives}
\item{fp}{false positives}
\item{sensitivity}{sensitivity}
\item{specificity}{specificity}
\item{npv}{negative predicted value, i.e. true negatives over true negatives and false positives}
\item{ppv}{positive predicted value, i.e. true positives over true positives and false negatives}
\item{wnpv}{weighted negative predicted value (an obscure measure)}
\item{wppv}{weighted positive predicted value}
\item{fpr}{false positive rate}
\item{fnr}{false negative rate}
\item{fdr}{false discovery rate}
\item{accuracy}{accuracy}
\item{f1}{F1 score}
\item{f2}{F2 score}
\item{f05}{F0.5 score}
\item{mcc}{Matthews correlation coefficient}
\item{informedness}{informedness}
\item{markedness}{markedness}
\item{auc}{AUC (area under the ROC curve estimated by
interpolating the (0,0), (1-specificity, sensitivity) and 
(1, 1) points in the ROC space)}
\item{gini}{the Gini index}
\item{n}{number of observations classified
<< end
}}
\references{Fawcett, Tom (2006). _An Introduction to ROC Analysis_. Pattern
Recognition Letters 27 (8): 861874.
doi:10.1016/j.patrec.2005.10.010.
Powers, David M W (2011). _Evaluation: From Precision, Recall and
F-Measure to ROC, Informedness, Markedness & Correlation._
Journal of Machine Learning Technologies ISSN: 2229-3981 &
ISSN: 2229-399X, Volume 2, Issue 1, 2011, pp-37-63
Available online at \url{http://www.bioinfo.in/contents.php?id=51}
}
\author{Tomas Sieger}



\seealso{\code{\link[ROCR]{performance}} which gives many more measures
}
\examples{
# example from https://en.wikipedia.org/w/index.php?title=Sensitivity_and_specificity&oldid=680316530
x<-matrix(c(20,10,180,1820),2)
print(x)
perfInd(x)

# compute perormance over a vector of predicted and true classification:
print(perfInd(c(0,0,1,1,1), c(0,0,0,1,1)))

# compare several measures over several classification results:
tbls<-list(matrix(c(98,2,2,8),2),matrix(c(8,2,2,8),2),matrix(c(80,20,2,8),2))
for (i in 1:length(tbls)) {
  m<-tbls[[i]]
  .pn(m)
  pi<-perfInd(m)
  with(pi,catnl(
    'acc: ',accuracy,
    ', sp+se: ',sensitivity+specificity,
    ', agr: ',m[2,2]/(sum(m)-m[1,1]),
    ', agr3: ',(2*sensitivity+specificity+ppv)/4,
    ', F1: ',f1,
    ', f2: ',f2))
}

# make degenerated table proper 2x2 table
i1<-c(1,1,1,1)
i2<-c(1,1,2,2)
m<-table(i1==1,i2==1)
print(m) # this is 1x2 table
perfInd(m)$table # this is 2x2 table
}
