refineFactors<-structure(
function # Refine factors in two twin data frames.
##description<<
## Refine two data frames 'x1' and 'x2' to be usable as a pair of
## a train/test set pair in a modeling or classification task, such
## that a model/classifier (e.g. a binomial GLM) can be trained on the
## train set and can be then directly applied to the test set.
## Note that had the sets not been refined this way, a model trained on
## a train set could not have been directly applied to the test set
## because a new factor level (not appearing in the train set) would
## have appeared in it, giving no clue how to predict response using
## such an unknown factor level.
##
##details<<
## Usually, the refinement consists of i) making the levels of factors
## in individual columns in each of the sets identical, and
## ii) removing columns containing factors of only a single level. This
## first task is achieved by removing rows in which appears a factor of
## level not appearing in the twin data frame, and dropping unused
## levels from factors.
##
##seealso<< 'dropLevels'
(x1, ##<< first data frame
x2, ##<< second data frame
unify = TRUE, ##<< shall factors be unified?
dropSingular = TRUE, ##<< shall factors having only a single level be
## removed?
naLimit = Inf, ##<< numeric columns containing more than 'naLimit' NA's
## (in 'x1' and in 'x2') will be converted into a factor created by
## 'cut'-ting the numeric values into 'k' intervals, and adding a
## special 'naLevelName' level to hold the missing values
k = 5, ##<< the number of intervals into which numeric columns having
## at least 'naLimit' NA's (in 'x1' and in 'x2') will be converted
naLevelName = "(NA)", ##<< the name of the special factor level
## used to represent missing values
verbose = FALSE, ##<< report progress?
debug = FALSE ##<< if TRUE, debugs will be printed. If numeric of value
## greater than 1, verbose debugs will be produced.
) {

  if (is.null(naLimit) || is.na(naLimit)) {
    naLimit<-Inf
  }
  if (naLimit<1) {
    naLimit<-1
  }

  # check that x1 and x2 are twins
  if (ncol(x1)!=ncol(x2)) {
    stop('the number of columns in \'x1\' and \'x2\' differ')
  }
  for (i in 1:ncol(x1)) {
    if (is.factor(x1[,i])) {
      if (!is.factor(x2[,i])) {
        stop(paste('factor',colnames(x1)[i],'in \'x1\' has no corresponding twin factor in \'x2\''))
      }
      if (length(levels(x1[,i]))!=length(levels(x2[,i])) || any(levels(x1[,i])!=levels(x2[,i]))) {
        stop(paste0('levels of factor \'',colnames(x1)[i],'\' incompatible: ',
          paste(levels(x1[,i]),collapse=' '),' vs. ',paste(levels(x2[,i]),collapse=' ')))
      }
    }
  }

  if (verbose) cat('pass 1 - replace NA\'s with a special factor level\n')
  if (is.finite(naLimit)) {
    for (i in 1:ncol(x1)) {
      if (verbose) cat(paste0('processing column ',i,': ',colnames(x1)[i],'\n'))
      if (!is.null(x1[,i])&&sum(is.na(x1[,i]))>=naLimit &&
        !is.null(x2[,i])&&sum(is.na(x2[,i]))>=naLimit) {
        if (is.numeric(x1[,i])) {
          if (verbose) paste(' converting to a factor with special \'',naLevelName,'\' level')
          tmp<-c(x1[,i],x2[,i])
          tmp<-cut(tmp,k)
          # reserve the first factor level for NA
          tmp2<-as.numeric(tmp)+1
          # put NA's there
          tmp2[is.na(tmp2)]<-1
          tmp<-factor(tmp2,levels=1:(1+length(levels(tmp))),labels=c(naLevelName,levels(tmp)))
          x1[,i]<-tmp[1:nrow(x1)]
          x2[,i]<-tmp[nrow(x1)+(1:nrow(x2))]
        } else {
          if (verbose) paste(' adding a special \'',naLevelName,'\' level')
          if (any(levels(x1[,i])=='(NA)')) {
            stop('column \'',colnames(x1)[i],'\' already contains the \'',naLevelName,
            '\' level, can\'t add this level, ',
            '- consider changing the \'naLevelName\' argument or the data')
          }
          tmp<-as.numeric(x1[,i])+1
          tmp[is.na(tmp)]<-1
          x1[,i]<-factor(tmp,levels=1:(length(levels(x1[,i]))+1),labels=c(naLevelName,levels(x1[,i])))
          tmp<-as.numeric(x2[,i])+1
          tmp[is.na(tmp)]<-1
          x2[,i]<-factor(tmp,levels=1:(length(levels(x2[,i]))+1),labels=c(naLevelName,levels(x2[,i])))
        }
      }
    }
  }

  if (verbose) cat('pass 2 - unify factor levels and drop columns of constant factor\n')
  if (!is.null(x1)) x1<-na.omit(x1)
  if (!is.null(x2)) x2<-na.omit(x2)

  columnsRemoved<-c()
  x1.orig<-x1
  x2.orig<-x2
  while (TRUE) {
    if (debug) .pn(columnsRemoved)
    x1<-x1.orig
    x2<-x2.orig
    if (length(columnsRemoved)>0) {
      x1<-x1[,-columnsRemoved,drop=FALSE]
      x2<-x2[,-columnsRemoved,drop=FALSE]
    }
    if (debug>1) {
      cat('x1 and x2 entering next iteration:\n')
      .pn(x1)
      .pn(x2)
    }
    dirty<-FALSE
    columnRemoved<-FALSE
    i<-1
    while (i<=ncol(x1)) {
      if (verbose) cat(paste0('processing column ',i,': ',colnames(x1)[i],'\n'))
      if (debug>1) {
        cat('x1 and x2 at this stage:\n')
        .pn(x1)
        .pn(x2)
      }
      dirty<-FALSE
      if (is.factor(x1[,i])) {

        # sanity check
        if (length(levels(x1[,i]))!=length(levels(x2[,i])) || any(levels(x1[,i])!=levels(x2[,i]))) {
          stop(paste0('levels of factor \'',colnames(x1)[i],'\' incompatible: ',
            paste(levels(x1[,i]),collapse=' '),' vs. ',paste(levels(x2[,i]),collapse=' ')))
        }

        f1<-x1[,i]
        f2<-x2[,i]

        if (dropSingular) {
          if (length(levels(dropLevels(f1)))<2 || length(levels(dropLevels(f2)))<2) {
            if (verbose) cat(' dropping this column due to singularity, restarting\n')
            # indices of columns in original x1
            idx<-1:(ncol(x1)+length(columnsRemoved))
            # remove columns that have already been removed
            if (length(columnsRemoved)>0) {
              idx<-idx[-columnsRemoved]
            }
            columnsRemoved<-c(columnsRemoved,idx[i])
            columnRemoved<-TRUE
            x1<-x1[,-i,drop=FALSE]
            x2<-x2[,-i,drop=FALSE]
            break
          }
        }

        if (unify) {
          sf1<-sapply(levels(f1),function(x)sum(f1==x,na.rm=T))
          sf2<-sapply(levels(f2),function(x)sum(f2==x,na.rm=T))
          if (verbose) {
            cat(' levels non-empty in x1?\n')
            print(sf1)
            cat(' levels non-empty in x2?\n')
            print(sf2)
          }
          ii<-which(sf1!=sf2)
          if (verbose) {
            cat(' indices that differ: ')
            print(ii)
          }
          if (length(ii)>0) {
            if (verbose) cat(' fixing\n')
            dirty<-TRUE
            for (iii in ii) {
              if (sf1[iii]>0) {
                if (verbose) cat('removing',levels(f1)[iii],'from x1\n')
                x1<-x1[!x1[,i]%in%levels(f1)[iii],,drop=FALSE]
              } else {
                if (verbose) cat('removing',levels(f1)[iii],'from x2\n')
                x2<-x2[!x2[,i]%in%levels(f1)[iii],,drop=FALSE]
              }
            }
          }
          if (nrow(x1)==0 || nrow(x2)==0) {
            # indices of columns in original x1
            idx<-1:(ncol(x1)+length(columnsRemoved))
            # remove columns that have already been removed
            if (length(columnsRemoved)>0) {
              idx<-idx[-columnsRemoved]
            }
            columnsRemoved<-c(columnsRemoved,idx[i])
            if (verbose) cat(' effectively dropping this column, restarting\n')
            columnRemoved<-TRUE
            break
          }
        }
        x1[,i]<-dropLevels(x1[,i],reorder=FALSE)
        x2[,i]<-dropLevels(x2[,i],reorder=FALSE)
      }
      if (dirty) {
        # something has been removed, and this could have affected
        # some already processed columns -> restart iterating over
        # the columns
        i<-1
      } else {
        i<-i+1
      }
    }
    if (debug>1) {
      cat('x1 and x2 after this iteration:\n')
      .pn(x1)
      .pn(x2)
    }
    if (!columnRemoved) break
  }

  res<-list(x1=x1,x2=x2)
  return(res)
  ### A list of refined 'x1' and 'x2',
},ex=function() {
  # unify factor levels and remove constant factors:
  x<-data.frame(x = 1:6, y = c('a','b','c','b','c','d'), z = c('d','c','c','c','c','d'))
  x.train <- x[1:3, ]
  x.test <- x[4:6, ]
  print(x.train)
  print(x.test)
  refineFactors(x.train, x.test)
  # Note: 'x[1,]' and 'x2[3,]' dropped because it had no counterpart
  #   in the twin data frame.
  # Note: 'x$z' dropped because after removal of 'x[1,]' and 'x2[3,]',
  #   there was only a single factor level left, which was dropped, by default.

  # unify factor levels but keep constant factors:
  refineFactors(x.train, x.test, dropSingular = FALSE)
  # Note: now 'x$z' is left

  # convert numeric columns with many NA's into a factor
  x<-data.frame(x = 1:10, y = c(NA,NA,1,2,3,NaN,NA,1,2,3), z = c(1,2,NA,4,5,1,2,NA,4,5))
  x1 <- x[1:5, ]
  x2 <- x[6:10, ]
  print(x1)
  print(x2)
  refineFactors(x1, x2, naLimit=2)

  # add a special 'NA' level to factors with many NA's
  x<-data.frame(x = 1:10, y = factor(c(NA,NA,1,2,3,NA,NA,1,2,3)), z = factor(c(1,2,NA,4,5,1,2,NA,4,5)))
  x1 <- x[1:5, ]
  x2 <- x[6:10, ]
  print(x1)
  print(x2)
  refineFactors(x1, x2, naLimit=2)

  # NaN in factors differs from NA - this would behave differently:
  # x<-data.frame(a = 1:10, b = factor(c(NA,NA,1,2,3,NA,NaN,1,2,3)), c=factor(c(1,2,NA,4,5,1,2,NA,4,5)))

})
