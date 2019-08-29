countFactorLevels<-structure(
function # Count the number of factor levels.
##description<<
## \code{\link{countFactorLevels}} summarizes a data frame, a list, or a factor by
## computing the number of declared and used factor levels in it (in
## case of factors) or its constituents (in case of data frames and
## lists).
(x ##<< a data frame, a list, or a factor
) {
  if (is.factor(x)) {
    x<-as.data.frame(x)
  } else if (!is.list(x)) {
    x<-as.list(x)
  }
  res<-sapply(x,function(x)ifelse (is.factor(x), length(levels(x)),NA))
  res<-rbind(res,sapply(x,function(x)ifelse (is.factor(x), length(levels(dropLevels(x))),NA)))
  rownames(res)<-c('all','used')
  res
  ### a data frame having the columns of \code{x} and two rows ('all',
  ### 'used') holding numbers of all (used) factor levels in
  ### individual columns of \code{x}
},ex=function() {
  if (require(MASS)) {
    countFactorLevels(Cars93)

    # omitting the first row results in some levels of \code{Cars$Model} and
    # \code{Cars93$Make} unused (and reported in the second row)
    countFactorLevels(Cars93[-1L, ])

    countFactorLevels(Cars93$Model[-1L])
    }
})
