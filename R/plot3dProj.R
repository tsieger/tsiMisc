plot3dProj<-structure(
function # Visualize multidimensional data in 3D and/or multiple 2D projections.
##description<<
## TODO
(x, ##<< a data frame or matrix to visualize. Instances in rows,
## features in columns. It must have at least 3 columns (dimensions).
col =  NULL, ##<< the color of individual instances
## color of individual instances, recycled if necessary
cls = NULL, ##<< class membership of individual instances. This serves
## the only purpose of alternative color specification: if
## 'col' is NULL ...
size = 3, ##<< size of points
alpha = 1, ##<< alpha of points
x2 = NULL, ##<< another data frame or matrix to visualize.
col2 =  NULL, ##<< the color of data in \code{x2}
cls2 = NULL, ##<< class membership of data in \code{x2}
size2 = 3, ##<< size of points in \code{x2}
alpha2 = 1, ##<< alpha of points in \code{x2}
palette = c('black','red','green','blue'), ##<< color palette to be
## used for individual classes specified in \code{cls}, see the
## \code{col} argument
main = NULL, ##<< title
scale = TRUE, ##<< if TRUE, data get scaled to the range of '[-1, 1]'
## in all dimensions
tx = function(y,center=TRUE) y-center*matrix(colMeans(x),nrow=nrow(y),ncol=ncol(x),byrow=TRUE), ##<< Transform
## function used to transform data from the k-dimensional space of 'x'
## into 3D space to visualize. The function must accept two arguments:
## a data matrix to transform, and a logical called \code{center}
## specifying whether to center the data prior the transform. The
## function must return a 3-column transformed version of the input
## data matrix.
k = ncol(x), ##<< if \code{tx} is one of \code{\link{txPca}} or
## \code{\link{txSpa}}, \code{k} can specify the number of dimensions
## to plot. The default is the number of dimensions of \code{x}, but
## smaller values are recommended for high-dimensional data in order to
## speed up plotting.
dimToShow = NULL, ##<< if \code{tx} is user-supplied, \code{dimToShow}
## can specify which dimensions of \code{x} to plot. \code{dimToShow}
## can be a numeric vector consisting of dimensions to show, or a
## character vector of the names of dimensions to show.
type = '3awm,sw', ##<<
col.axes = 'gray', ##<< color of axes in the 3D plot
axesExpansion = 1.1, ##<<
annotate=FALSE,
debug = FALSE ##<< if TRUE, debugs will be printed. If numeric of value
## greater than 1, verbose debugs will be produced.
) {
  #library(rgl) # open3d,mfrow3d,spheres3d,lines3d,text3d
  #library(e1071) # bincombinations
  #library(geometry) # convhulln
  #library(tsiMisc) # vectorprod

  if (!is.matrix(x)) x<-as.matrix(x)
  if (!is.null(cls)) cls<-as.factor(cls)

  # check the \code{tx} argument
  if (is.null(tx)) {
    stop('need an \'tx\' argument')
  }
  if (length(formals(tx))!=2 || names(formals(tx))[2]!='center') {
    stop('\'tx\' must take two arguments, and the second must be named \'center\'')
  }
  tmp<-tx(x[1,,drop=FALSE],center=TRUE)
  if (!inherits(tmp,'matrix')) {
    stop('invalid \'tx\' argument: it does not produce a matrix ',
      '(maybe you forgot to add \'drop=FALSE\' when indexing your matrix')
  }

  # dimensionality of \code{x}
  k0<-ncol(x)
  if (k0<3 || k<3) {
    stop('we need at least 3D data')
  }
  if (ncol(tx(x[1,,drop=FALSE],center=TRUE))!=3) {
    stop('\'tx\' must result in 3D data')
  }

  if (debug) .pn(k0)
  if (debug) .pn(k)

  if (k<k0) {
    # user requested to draw fewer than \code{k0} dimensions
    # determine which dimensions to omit
    if (!is.null(dimToShow)) {
      if (debug) {
        cat('guessing which dimensions to show from \'dimToShow\' argument')
        .pn(dimToShow)
      }
      if (is.numeric(dimToShow)) {
        if (!all(dimToShow %in% 1:k0) || anyDuplicated(dimToShow)) {
          stop('invalid \'dimToShow\' argument, expected unique numbers in the range of 1 to',k0)
        }
        dimVisible<-(1:k0)%in%dimToShow
      } else {
        dimVisible<-colnames(x)%in%dimToShow
      }
      if (sum(dimVisible)<3) {
        stop('\'dimToShow\' argument selects ',sum(dimVisible),
          ' dimension(s), and we need at least 3 for a 3D plot')
      }
    } else {
      if (!is.null(attr(tx,'params')) &&
        !is.null(attr(tx,'params')$varExplained) &&
        is.function(attr(tx,'params')$varExplained)) {
        varExpl<-attr(tx,'params')$varExplained(k)
        if (debug) {
          cat('deviance in first k dimensions:\n')
          .pn(varExpl)
        }
        dimVisible<-varExpl>=sort(varExpl,decreasing=TRUE)[k]
      } else {
        stop('can\'t determine which dimensions to show: missing \'dimToShow\' ',
          'argument and \'tx\' does not contain \'varExplained\' function')
      }
    }
  } else {
    dimVisible<-rep(TRUE,k0)
  }
  if (debug) .pn(dimVisible)
  dimVisibleIdx<-which(dimVisible)
  if (debug) .pn(dimVisibleIdx)

  if (is.null(col) || length(col)==0) {
    if (!is.null(cls) && !is.null(palette)) {
      col<-palette[cls]
    } else {
      col<-'black'
    }
  }
  if (!is.null(x2)) {
    if (is.null(col2) || length(col2)==0) {
      if (!is.null(cls2) && !is.null(palette)) {
        col2<-palette[cls2]
      } else {
        col2<-'black'
      }
    }
  }
  
  if (scale) {
    x<-scaleToUnit(x,-1,1)
    if (!is.null(x2)) {
      scalingTx<-attr(x,'scaleToUnit:tx')
      stopifnot(scalingTx!=NULL)
      x2<-scalingTx(x2)
    }
  }

  doPlotWireFrame<-stringr::str_detect(type,'w')
  doPlotScatters<-stringr::str_detect(type,'s')

  buildWireFrame<-function() {
    wireFrame<-matrix(NA,nrow=3*k*2^(k-1),ncol=3)
    bs<-e1071::bincombinations(k-1)
    cnt<-0
    for (i in 1:k) {      # 'i' iterates over 'dimVisibleIdx'
      b1<-rep(.5,k0) # .5 for invisible dimensions
      b2<-b1
      b1[dimVisibleIdx[i]]<-0
      b2[dimVisibleIdx[i]]<-1
      for (bi in 1:nrow(bs)) {
        b1[dimVisibleIdx[-i]]<-b2[dimVisibleIdx[-i]]<-bs[bi,]
        if (signatureInConvhull[signature(b1)]&&signatureInConvhull[signature(b2)]) {
          v1<--1+2*b1
          v2<--1+2*b2
          wireFrame[cnt+1,]<-tx(rbind(v1),center=FALSE)
          wireFrame[cnt+2,]<-tx(rbind(v2),center=FALSE)
          # reserve lines of NA's
          cnt<-cnt+3
          #lines3d(rbind(tx(rbind(v1)),tx(v2)),color=c('gray'))
        }
        #lines3d(rbind(tx(v1),tx(rbind(v2))),color=c('gray','blue')[(in.convhull(v1)&in.convhull(v2))+1])
      }
    }
    wireFrame<-wireFrame[1:cnt,]
    return(wireFrame)
  }

  if (doPlotScatters || doPlotWireFrame) {
    # compute coordinates of all vertices of the individual axes in the untransformed space
    bs0<-e1071::bincombinations(k)
    bs0<-bs0[,ncol(bs0):1]
    # make sure we got what we expected,
    # each pair of vertices must differ just in the first coordinate
    stopifnot(bs0[1,-1]==bs0[2,-1])
    # supplement dimensions to show with zeros for hidden dimensions
    # (insert zero columns at invisible dimensions)
    bs<-matrix(.5,nrow(bs0),k0)
    i<-1
    for (i0 in 1:k0) {
      if (dimVisible[i0]) {
        bs[,i0]<-bs0[,i]
        i<-i+1
      }
    }
    # Now we consider a 0/1 vector as a binary number, and
    # this way characterize the vector by a single number.
    # (Note we convert .5 (standing for hidden dims) to 0 when
    # computing the signature.)
    signature<-function(x) 1+crossprod(x==1,2^((length(x)-1):0))
    bSignatures<-apply(bs,1,signature)
    vs<--1+2*bs
    if (debug>1) .pn(vs)
    # find convex hull - only points on the convex hull will be visible
    idx.convhull<-unique(as.numeric(geometry::convhulln(tx(rbind(vs),center=FALSE))))
    #sort(idx.convhull)
    idxInConvhull<-1:nrow(vs)%in%idx.convhull
    signatureInConvhull<-rep(FALSE,2^k0)
    signatureInConvhull[bSignatures[idxInConvhull]]<-TRUE
    if (debug>1) .pn(signatureInConvhull)
  }
  if (doPlotWireFrame) {
    wf<-buildWireFrame()
    if (debug>1) .pn(wf)
  }


  ###
  ## plotting functions
  plotAxes<-function() {
    if (!is.null(colnames(x))) {
      v0<-rep(0,k0)
      axes.radius<-c(.01,.01,.03,0)
      for (i in 1:k) {
        v1<-v2<-v3<-v4<-rep(0,k0)
        v1[dimVisibleIdx[i]]<-.95
        v2[dimVisibleIdx[i]]<-.95
        v3[dimVisibleIdx[i]]<-1
        v4[dimVisibleIdx[i]]<-1.1
        shade3d(cylinder3d(tx(axesExpansion*rbind(v0,v1,v2,v3),center=FALSE),
          radius=axes.radius,closed=-2),col=col.axes)
          #,emission=col.axes,specular=col.axes,alpha=1,shininess=0)
        text3d(tx(axesExpansion*rbind(v4),center=FALSE),
          texts=colnames(x)[dimVisibleIdx[i]],col=col.axes)
          #,emission=col.axes,specular=col.axes,alpha=1,shinness=0)
      }
    }
  }

  # plot wire frame
  plotWireFrame<-function(annotate=FALSE) {
    lines3d(wf,color='lightgray',alpha=.5)
    #lines3d(wf,color='lightgray',emission='lightgray',
    #specular='lightgray',ambient='lightgray',alpha=.5,shiness=50)

    if (annotate) {
      # mark point (-1,-1,-1,...,-1)
      v<-rep(.5,k0)
      v[dimVisibleIdx]<--1
      points3d(tx(rbind(v,center=FALSE)),color='black',size=10)
      # mark axes pointing away from (-1,-1,-1,...,-1,0,0,0)
      for (i in 1:k) {
        v<-rep(.5,k0)
        v[dimVisibleIdx]<--1
        v[dimVisibleIdx[i]]<-1
        text3d(tx(rbind(v),center=FALSE),texts=i)
      }
    }
  }

  # plot 3D scatter plot of observations
  plot3dScatter<-function(x,size,col,alpha) {
    #spheres3d(tx(x),radius=radius,color=col,alpha=alpha)
    if (length(unique(size))==1) {
      points3d(tx(x,!scale),size=size,color=col,alpha=alpha)
    } else {
      tmp.size<-rep(size,length=nrow(x))
      tmp.col<-rep(col,length=nrow(x))
      tmp.alpha<-rep(alpha,length=nrow(x))
      for (s in unique(size)) {
        points3d(tx(x[tmp.size==s,],!scale),size=s,
          color=tmp.col[tmp.size==s],alpha=tmp.alpha[tmp.size==s])
      }
    }
  }

  # plot a projection of k-dimensional parallelepipedon having scatter
  # plots on its faces
  plotScatters<-function() {
    textureFileName<-tempfile('texture',fileext='.png')
    for (i1 in 1:(nrow(vs)-2)) {
      if (i1 %in% idx.convhull) {
        v1<-vs[i1,]
        for (i2 in (i1+1):(nrow(vs)-1)) {
          v2<-vs[i2,]
          if (i2 %in% idx.convhull && sum(v1!=v2)==1) {
            for (i3 in (i2+1):nrow(vs)) {
              v3<-vs[i3,]
              if (i3 %in% idx.convhull && sum(v1!=v2)==1 &&
                sum(v1!=v3)==1 && sum(v2!=v3)==2) {
                #cat(sprintf('%d %d %d\n',i1,i2,i3))
                v4<-v2+v3-v1
                i4<-which(apply(vs,1,function(x)all(v4==x)))
                if (!i4 %in% idx.convhull) next
                # order the vertices such that the face points towards inside
                if (tcrossprod(vectorprod(
                  tx(rbind(v1),center=FALSE),
                  tx(rbind(v2),center=FALSE)),
                  tx(rbind(v4),center=FALSE))<0) {
                  w1<-v1
                  w2<-v2
                  w4<-v4
                  w3<-v3
                } else {
                  w1<-v1
                  w2<-v3
                  w4<-v4
                  w3<-v2
                }
                # create a scatter plot texture 
                png(textureFileName)
                opar<-par(mar=c(6,5,4,2)+.1, ask=FALSE)
                idx1<-which(w2-w1!=0)
                idx2<-which(w3-w1!=0)
                plot(x[,c(idx1,idx2)],pch=19,frame=F,col=col,cex.lab=3)
                #,xlim=c(-1,1),ylim=c(-1,1))
                #text(0,.5,paste(idx1,idx2),cex=8)
                #text(0,-.5,paste(crossprod(vectorprod(v1,v2),v4)<0,
                #  paste(w2-w1,collapse=' '),paste(w3-w1,collapse=' '),sep=','),cex=3)
                par(opar)
                dev.off()
                # plot the texture
                #rgl.quads(tx(rbind(w1,w2,w4,w3)),texture=textureFileName,alpha=.5,
                #  texcoord=rbind(c(0,0),c(1,0),c(1,1),c(0,1)),lit=FALSE,front='fill',back='cull')
                rgl.quads(tx(rbind(w1,w2,w4,w3),center=FALSE),
                  texture=textureFileName,
                  alpha=1,texcoords=rbind(c(0,0),c(1,0),c(1,1),c(0,1)),
                  lit=TRUE,shininess=100,front='fill',back='cull',
                  ambient=gray(.5),specular='black')
              }
            }
          }
        }
      }
    }
    #unlink(textureFileName)
  }

  plotTitle<-function() {
    decorate3d(main=main,box=FALSE,axes=FALSE)
  }

  #type = '3aw,sw', '3,s;a'

  scenes<-c()
  # split to scenes
  sceneTypes<-str_split(type,'\\|')[[1]]
  for (i in seq(along=sceneTypes)) {
    sceneType<-sceneTypes[i]

    if (debug) cat(sprintf('plotting scene "%s"\n',sceneType));
    #if (i>1 || rgl.cur()==0) {
      scenes<-c(scenes,open3d())
    #}
    # split to rows of subscenes
    subsceneRowTypes<-str_split(sceneType,';')[[1]]
    # how many columns?
    maxRowSize<-0
    for (subsceneRowType in subsceneRowTypes) {
      subsceneTypes<-str_split(subsceneRowType,',')[[1]]
      maxRowSize<-max(maxRowSize,length(subsceneTypes))
    }
    if (debug) {
      cat(sprintf('opening %d x %d subscenes\n',
        length(subsceneRowTypes),maxRowSize))
    }
    mfrow3d(length(subsceneRowTypes),maxRowSize,sharedMouse=T)
    firstSubscene<-TRUE
    for (subsceneRowType in subsceneRowTypes) {
      if (debug) cat(sprintf(' plotting subscene row "%s"\n',subsceneRowType));
      subsceneTypes<-str_split(subsceneRowType,',')[[1]]
      for (subsceneType in subsceneTypes) {
        if (debug) cat(sprintf('  plotting subscene "%s"\n',subsceneType));
        if (firstSubscene) {
          firstSubscene<-FALSE
        } else {
          if (debug>1) cat('  calling next3d()\n')
          next3d(reuse=FALSE)
        }
        plotTypes<-str_split(subsceneType,'')[[1]]
        for (plotType in plotTypes) {
          if (debug) cat(sprintf('    seen plotType "%s"\n',plotType));
          if (length(plotType)>0) {
            switch(plotType,
              'a'=plotAxes(),
              'w'=plotWireFrame(),
              '3'=plot3dScatter(x,size,col,alpha),
              '4'=plot3dScatter(x2,size2,col2,alpha2),
              's'=plotScatters(),
              'm'=plotTitle(),
              'otherwise'=stop('unknown type "',plotType,'"'))
          }
        }
      }
    }
  }
  rglSetMouseCbTrackball(scenes)

},ex=function() {
  if (interactive() && require(rgl)) {
    plot3dProj(iris[,1:3], cls=iris$Species)
  }

# TODO: unify/propagate scaling done in plot3dProj here ?!
#  p <- prcomp(iris[, 1:4], center = TRUE, scale = TRUE)
#  tx <- function (y,center=TRUE) {
#    if(center) y<-y-matrix(colMeans(iris[, 1:4]),nrow=nrow(y),ncol=4,byrow=TRUE)
#    y<-t(t(p$rotation[,1:3])%*%t(y))
#    return (y)
#  }
#  plot3dProj(iris[,1:4], tx=tx, cls=iris$Species)

})
