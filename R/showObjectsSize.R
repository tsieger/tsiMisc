.sos<-showObjectsSize<-structure(
function # R object size dump.
##description<<
## \code{showObjectsSize} shows how much memory do objects in specific
## environment(s) occupy. It builds upon \code{\link{object.size}} and
## gathers information about the size and mode of objects in specific
## environment(s). It supports environment name filtering using regular
## expressions, object mode filtering, as well as filtering by size.
##
##details<<
## Partially based on
## \url{http://jeromyanglim.blogspot.cz/2009/11/memory-management-in-r-few-tips-and.html}.
##
##alias<< \code{.sos}
##
##seealso<< \code{\link{object.size}}, \code{\link{ls}}, \code{\link{mode}}
(mode = NULL, ##<< mode of objects to show (see 'mode'), use code{NULL} to
##  show objects of all modes.
n = 20, ##<< number of objects to show (the first \code{n} biggest objects
## will be shown).
env = parent.frame(), ##<< environment to operate in.
recursive = FALSE, ##<< shall we iteratively look in parent environments?
envNameRegexpr = NULL, ##<< regular expression character filter applied
## to the names of environments. If not \code{NULL}, only environments
## matched by \code{envNameRegexpr} are considered.
all.names = TRUE, ##<< if \code{TRUE}, all object names are returned.
## If \code{FALSE}, names which begin with a '.' are omitted.
sizeUnits = 1024*1024, ##<< the units of size (defaults to 1MB)
verbose = FALSE ##<< if \code{TRUE}, progress is reported.
) {
  res<-NULL
  while (TRUE) {
    if (verbose) cat(paste0('looking in env ',environmentName(env),'\n'))
    skipThisEnv<-FALSE
    if (!is.null(envNameRegexpr)) {
      if (regexpr(envNameRegexpr,environmentName(env))==-1) {
        skipThisEnv<-TRUE
      }
    }
    if (!skipThisEnv) {
      rv<-base::ls(env,all.names=all.names)
      sizes<-vector(mode='numeric',length(rv))
      types<-vector(mode='character',length(rv))
      modes<-vector(mode='character',length(rv))
      classes<-vector(mode='character',length(rv))
      rows<-vector(mode='numeric',length(rv))
      cols<-vector(mode='numeric',length(rv))
      for (i in seq(along=rv)) {
        o<-base::get(rv[i],envir=env)
        sizes[i]<-object.size(o)
        classes[i]<-paste(class(o),collapse=' ')
        #obj.class <- napply(names, function(x) as.character(class(x))[1])
        #if (exists(rv[i],envir=sys.frame(-1),mode='numeric')) {
        #  types[i]<-'numeric'
        #} else if (exists(rv[i],envir=sys.frame(-1),mode='function')) {
        #  types[i]<-'function'
        #} else if (exists(rv[i],envir=sys.frame(-1),mode='language')) {
        #  types[i]<-'language'
        #} else {
        #  types[i]<-'other'
        #}
        types[i]<-typeof(o)
        modes[i]<-mode(o)
        if (is.null(nrow(o))) {
          if (modes[i]!="function") {
            rows[i]<-length(o)
          } else {
            rows[i]<-NA
          }
        } else {
          rows[i]<-nrow(o)
        }
        if (is.null(ncol(o))) {
          cols[i]<-NA
        } else {
          cols[i]<-ncol(o)
        }
      }
      res<-c(res,list(list(name=rv,size=sizes,type=types,mode=modes,class=classes,row=rows,col=cols,env=environmentName(env))))
    }
    if (recursive) {
      env<-parent.env(env)
      if (identical(env,emptyenv())) break
    } else {
      break
    }
  }
  tmp<-sapply(res,function(x)length(x$size))
  cnt<-ifelse(is.numeric(tmp),sum(tmp),0)
  if (verbose) cat(cnt,'objects found\n')
  names<-vector(mode='character',cnt)
  sizes<-vector(mode='numeric',cnt)
  types<-vector(mode='character',cnt)
  modes<-vector(mode='character',cnt)
  classes<-vector(mode='character',cnt)
  rows<-vector(mode='numeric',cnt)
  cols<-vector(mode='numeric',cnt)
  envs<-vector(mode='character',cnt)

  # merge results found in individual environments into a single piece
  i<-0
  for (j in seq(along=res)) {
    k<-length(res[[j]]$name)
    if (k>0) {
      names[i+(1:k)]<-res[[j]]$name
      sizes[i+(1:k)]<-res[[j]]$size
      types[i+(1:k)]<-res[[j]]$type
      modes[i+(1:k)]<-res[[j]]$mode
      classes[i+(1:k)]<-res[[j]]$class
      rows[i+(1:k)]<-res[[j]]$row
      cols[i+(1:k)]<-res[[j]]$col
      envs[i+(1:k)]<-res[[j]]$env
      i<-i+k
    }
  }
  rv<-data.frame(name=names,size=sizes/sizeUnits,type=types,mode=modes,class=classes,rows=rows,cols=cols,env=envs)

  # sort
  rv<-rv[order(rv$size,decreasing=TRUE),]

  # filter by mode
  if (!is.null(mode)) rv<-rv[rv$mode==mode,]

  attr(rv,'totalSize')<-sum(rv$size)

  # limit the number of objects returned
  if (!is.null(n)) rv<-rv[rseq(1,min(nrow(rv),n),1),]

  return(rv)
  ### A data frame of columns \code{name}, \code{size}, \code{mode},
  ### and \code{env}.
  ### In addition, the total size of all objects found is put into
  ### the \code{totalSize} attribute of the data frame.
},ex=function() {
  showObjectsSize(n=20)

  if (require('MASS')) {
    showObjectsSize(mode = 'numeric', envNameRegexpr = glob2rx('package:MASS'), rec = TRUE)
  }
})
