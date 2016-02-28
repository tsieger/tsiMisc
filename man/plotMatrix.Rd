\name{plotMatrix}
\alias{plotMatrix}
\title{Plot a matrix.}

\usage{plotMatrix(x, col = gray.colors(16), transpose = FALSE, axes = c(2, 
    3), legend = TRUE, legend.width = 0.2, legend.nlevels = length(col) + 
    1, rowDelim = NULL, rowDelimCol = "black", colDelim = NULL, 
    colDelimCol = "black")}
\arguments{
  \item{x}{a matrix}
  \item{col}{colors}
  \item{transpose}{transpose the matrix?}
  \item{axes}{which axes (sides of the matrix) to annotate?
(1=below, 2=left, 3=above and 4=right).}
  \item{legend}{draw legend?}
  \item{legend.width}{the relative width of the legend to the total
width of the plot}
  \item{legend.nlevels}{number of levels in the legend
(defaults to the number of colors in 'col')}
  \item{rowDelim}{an optional vector of rows after which to put a
separation line}
  \item{rowDelimCol}{the color of the row separation line}
  \item{colDelim}{an optional vector of columns after which to put
a separation line}
  \item{colDelimCol}{the color of the column separation line}
}
\details{'plotMatrix' plots a matrix, decorated with the names
of rows and columns, and, if requested, with lines separating
specified rows/columns. Also, a legend gets drawn, by default.}


\author{Tomas Sieger}




\examples{
x<-matrix(1:36,6)
rownames(x)<-paste('row',1:6)
colnames(x)<-paste('column',1:6)
x
plotMatrix(x)

# group rows and columns
x<-repmat(matrix(3-c(1,2,1,2,3,2,1,2,1),3),3,3)
plotMatrix(x, rowDelim = c(3,6), colDelim = c(3,6), colDelimCol='red')
}
