\name{perfInd}
\alias{perfInd}
\title{Compute performance indicators.}

\usage{perfInd(x, negativeFirst = FALSE)}
\arguments{
  \item{x}{a 2x2 classification table having predictions in rows, and
ground truth in columns}
  \item{negativeFirst}{if TRUE, positive case come first in both
rows and columns}
}
\details{Compute several indicators to describe the performance of a binary
classifier, e.g. sensitivity, specificity, an estimate of the area
under the receiver operating characteristic, the Gini coefficient
etc. (see the return value).

<<seealso ROCR::performance which gives many more measures

<<references
Fawcett, Tom (2006). _An Introduction to ROC Analysis_. Pattern
Recognition Letters 27 (8): 861874.
doi:10.1016/j.patrec.2005.10.010.
Powers, David M W (2011). _Evaluation: From Precision, Recall and
F-Measure to ROC, Informedness, Markedness & Correlation._
Journal of Machine Learning Technologies ISSN: 2229-3981 &
ISSN: 2229-399X, Volume 2, Issue 1, 2011, pp-37-63
Available online at http://www.bioinfo.in/contents.php?id=51}
\value{A row matrix containing named entries of 'sensitivity',
'specificity', 'npv' (negative predictive value), 'ppv',
'wnpv' (weighted npv, an obscure measure), 'wppv',
'fpr' (falso positive rate), 'fnr', 'fdr' (false discovery rate),
'acc' (accuracy), 'f1', 'auc' (area under the ROC curve estimated
by interpolating the (0,0), (1-specificity, sensitivity) and 
(1, 1) points in the ROC space), 'gini' (the Gini index), and 'n'
(number of classified cases).}

\author{Tomas Sieger}




\examples{
# example from https://en.wikipedia.org/w/index.php?title=Sensitivity_and_specificity&oldid=680316530
x<-matrix(c(20,10,180,1820),2)
print(x)
perfInd(x)
}
