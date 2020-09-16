rencol<-structure(
function # Rename columns in a data frame.
##description<<
## \code{\link{rencol}} renames columns in a data frame.
##
(d, ##<< data frame
on, ##<< original column name (or a regexpr)
nn, ##<< desired column name (or a regexpr)
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
    if (length(grep(paste0(on,'.*'),colnames(d)))==0) {
      stop('no columns match the pattern "',on,'"')
    }
    colnames(d)<-gsub(on,nn,colnames(d))
  } else {
    if (!is.null(i)) {
      if (!is.numeric(i) || length(i)!=1 || i>length(colnames(d))) {
        stop('invalid index i')
      }
      if (grep(on,colnames(d)[i])!=1) {
        stop(paste0('colname at index ',i,': "',colnames(d)[i],'" does not match "',on,'"'))
      }
      colnames(d)[i]<-nn
    } else {
      ii<-grep(on,colnames(d))
      if (length(ii)>1) {
        stop(paste0('several columns match "',on,'"'))
      }
      if (length(ii)==0) {
        stop(paste0('no column matches "',on,'"'))
      }
      colnames(d)[ii]<-nn
    }
  }
  return(d)
},ex=function() {
  tmp<-data.frame(a=1:2,b=2:3)
  rencol(tmp,'a','a2')
  rencol(tmp,'a','a2',1)

  # get rid of a common prefix
  tmp<-data.frame(prefix.a=1:2,prefix.b=2:3)
  rencol(tmp,'prefix.','',multi=TRUE)
})
