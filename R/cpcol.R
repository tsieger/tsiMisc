cpcol<-structure(
function # Copy columns in a data frame.
##description<<
## \code{\link{cpcol}} copies columns in a data frame.
##
##seealso<< rencol
(d, ##<< data frame
on, ##<< original column name (or part of it, or a regexpr)
nn, ##<< desired column name (or part of it, or a regexpr)
i = NULL, ##<< optional column index that must match the column with the name
## denoted by \code{on} (this serves as a sanity check ensuring the
## change affects the intended column)
regexp = FALSE, ##<< logical flag indicating whether \code{on} and
## \code{nn} contain regular expressions
multi = FALSE ##<< logical flag indicating whether to change multiple
## coumns at once
) {
  if (is.null(d)) return(d)

  if (!regexp) {
    on<-gsub('\\(','\\\\(',on)
    nn<-gsub('\\(','\\\\(',nn)
    on<-gsub('\\)','\\\\)',on)
    nn<-gsub('\\)','\\\\)',nn)
    on<-gsub('\\.','\\\\.',on)
    nn<-gsub('\\.','\\\\.',nn)
  }

  if (multi) {
    idx<-grep(paste0(on,'.*'),colnames(d))
    if (length(idx)==0) {
      stop('no columns match the pattern "',on,'"')
    }
    for (ii in idx) {
      d<-cbind(d,d[,ii],stringsAsFactors=FALSE)
    }
    cn<-colnames(d)
    cn[(ncol(d)-length(idx)+1):ncol(d)]<-gsub(on,nn,cn[idx])
    colnames(d)<-cn
  } else {
    if (!is.null(i)) {
      if (!is.numeric(i) || length(i)!=1 || i>length(colnames(d))) {
        stop('invalid index i')
      }
      if (grep(on,colnames(d)[i])!=1) {
        stop(paste0('colname at index ',i,': "',colnames(d)[i],'" does not match "',on,'"'))
      }
      d<-cbind(d,d[,i],stringsAsFactors=FALSE)
      colnames(d)[ncol(d)]<-nn
    } else {
      ii<-grep(on,colnames(d))
      if (length(ii)>1) {
        stop(paste0('several columns match "',on,'"'))
      }
      if (length(ii)==0) {
        stop(paste0('no column matches "',on,'"'))
      }
      d<-cbind(d,d[,ii],stringsAsFactors=FALSE)
      colnames(d)[ncol(d)]<-nn
    }
  }
  return(d)
},ex=function() {
  tmp<-data.frame(a=1:2,b=2:3)
  cpcol(tmp,'a','a2')
  # copy with explicit column idx check
  cpcol(tmp,'a','a2',1)

  # copy all columns starting with 'a.' as 'b.'
  tmp<-data.frame(a.x=1:2,a.b=2:3)
  cpcol(tmp,'a.','b.',multi=TRUE)
})
