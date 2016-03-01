dropLevels<-structure(
function # Drop unused levels in a factor.
##description<<
## This is only a wrapper for 'gdata::drop.levels'.
##seealso<< 'gdata::drop.levels'
(x, ##<< object to be processed
reorder = TRUE, ##<< should factor levels be reordered using 'reorder.factor'?
... ##<< additional arguments to 'reorder.factor'
) {
  return(gdata::drop.levels(x,reorder,...))
  ### Input object without unused levels.
},ex=function() {
  f<-factor(c(NA,1,1,2),levels=c(0,1,2),labels=c('a','b','NA'))
  dropLevels(f)
})
