plot.default<-structure(
function # 'plot.default' with default plotting character 19 and no frame.
###
##<< details
(..., ##<< 
pch = 19, ##<< 
frame = FALSE
) {
  # we need this to get to the class of arguments
  dots<-list(...)
  # and this to save the names of arguments, i.e. not to expand the
  # arguments
  args<-match.call(expand.dots=FALSE)$...
  # this is necessary, args must not be pairlist to call do.call
  args<-as.list(args)

  if (!'pch' %in% names(args)) {
    args<-c(args,list(pch=pch))
  }
  if (!'frame' %in% names(args)) {
    args<-c(args,list(frame=frame))
  }

  do.call(graphics::plot.default,args,envir=parent.frame())
},ex=function() {
  with(iris,plot(Sepal.Length, Sepal.Width))
})
