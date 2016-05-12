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
main = NULL, ##<< title of \code{x}
x2 = NULL, ##<< another data frame or matrix to visualize.
col2 =  NULL, ##<< the color of data in \code{x2}
cls2 = NULL, ##<< class membership of data in \code{x2}
size2 = 3, ##<< size of points in \code{x2}
alpha2 = 1, ##<< alpha of points in \code{x2}
main2 = NULL, ##<< title of \code{x2}
x3 = NULL, ##<< another data frame or matrix to visualize.
col3 =  NULL, ##<< the color of data in \code{x3}
cls3 = NULL, ##<< class membership of data in \code{x3}
size3 = 3, ##<< size of points in \code{x3}
alpha3 = 1, ##<< alpha of points in \code{x3}
main3 = NULL, ##<< title of \code{x3}
palette = c('black','red','green','blue'), ##<< color palette to be
## used for individual classes specified in \code{cls}, see the
## \code{col} argument
scale = TRUE, ##<< if TRUE, data get scaled to the range of '[-1, 1]'
## in all dimensions
tx = function(y) y, ##<< Transform function used to transform data from
## the k-dimensional space of \code{x} into 3D space to visualize.
## The function takes a data matrix to transform, and is expected to
## return a 3-column transformed version of the input data matrix.
dimToShow = NULL, ##<< a numeric or character vector of dimensions
## of \code{x} to plot. If numeric, it indexes the dimensions to show.
## If character, it lists  the names of dimensions to show.
## The default it to plot all dimensions. \code{dimToShow} takes
## precedence over \code{k}, another way to specify the dimensions to
## show.
k = NULL, ##<< if \code{tx} is one of \code{\link{txPca}} or
## \code{\link{txSpa}}, \code{k} can specify the number of (most
## "significant") dimensions to plot. The default is the number of
## dimensions of \code{x}, but smaller values are recommended for
## high-dimensional data in order to speed up plotting. Internally, the
## \code{k} dimensions selected to be shown are those which contribute
## the most to the top \code{k} components (principal or supervised
## principal components) of the data. To determine this contribution,
## the code calls the \code{varExplained} stored as a name member of a
## list attached to the \code{tx} argument as the \code{params}
## attribute. This function takes \code{k}, the number of top
## components, and returns the contribution of individual dimensions to
## these components.
type = 'sawm,fw', ##<< a character string defining the type of plots to
## produce. There general syntax is \code{<row>;<row>;...;<row>},
## where each \code{<row>} stands for \code{<plot>,<plot>,...,<plot>},
## and each \code{plot} is a concatenation of one or more plot types to
## be combined in the specific plot. The plot types are:
## \itemize{
##   \item \code{a} - \strong{a}xes of the feature space
##   \item \code{b} - \strong{b}ox(es) as defined by the \code{boxes}
##             argument
##   \item \code{B} - wire frame \strong{B}ox(es) as defined by the
##         \code{wfboxes} argument
##   \item \code{d} - \strong{d}ecoration of a 3D plot (axes and
##             a bounding box, see \code{\link[rgl]{decorated3d}})
##   \item \code{e} - \strong{e}ellipse(s) as defined by the
##             \code{ellipses} argument
##   \item \code{f} - 2D scatter plots on the \strong{f}aces of a
##             parallelepipedon
##   \item \code{m} - \strong{m}ain (title) of data
##   \item \code{p} - \strong{p}lane(s) as defined by the \code{planes}
##             argument
##   \item \code{s} - 3D \strong{s}catter plot
##   \item \code{t} - \strong{t}ext(s) as defined by the \code{texts}
##             argument
##   \item \code{w} - \strong{w}ire frame defining the feature space
##             (the wireframe collides with the edges of the 
##             parallelepipedon plotted by \code{f})
##  }
## Each of \code{t}, \code{s}, and \code{m} can be followed by a number
## referring to a specific data (i.e. \code{x}, \code{x2}, or
## \code{x3}). By default, when no number follows the plot type, the
## \code{x} is assumed.
## For example: \code{tawm,sw} defines two subscenes to be plotted in
## one row next to each other. The first plot type \code{tawm} defines
## a \strong{t}hree dimensional scatter plot annotated with
## \strong{a}xes and enriched with a \strong{w}ire frame and the
## \strong{m}ain (title).
## The second plot type \code{sw} defines a parallelepipedon having
## \strong{s}catter plots on its faces, and enriched with a
## \strong{w}ire frame.
widths = 1, ##<< relative widths of columns in a multi-subscene scene,
## see \code{\link[rgl]{layout3d}}
heights = 1, ##<< relative heights of rows in a multi-subscene scene,
## see \code{\link[rgl]{layout3d}}
devices = rgl.cur(), ##<< a list of devices to plot at, defaulting to
## the current active device, if any. If \code{devices} is NULL, empty,
## contains invalid devices, or does not hold enough devices to plot
## all the scenes requested by the \code{type} argument, new scene(s)
## will be created for such scenes.
col.axes = 'gray', ##<< color of axes in the 3D plot
axesExpansion = 1.1, ##<<
annotateWireFrame = FALSE, ##<< if \code{TRUE}, the wire frame produced
## by \code{type='w'} will be annotates by a black point at
## \code{(-1,-1,...,-1)} and numbers 1...\code{k} at the extremes of
## the individual \code{k} dimensions.
ellipses = list(list(center=rep(0,ncol(x)), x=diag(rep(1,ncol(x))), col='gray', alpha=.2)), ##<< a
## list or a list of lists defining ellipses to be plotted by the
## \code{'e'} type. TODO
boxes = list(list(center=rep(0,ncol(x)), scale=rep(0,ncol(x)), col='gray', alpha=.2)), ##<< a
## list or a list of lists defining boxes to be plotted by the
## \code{'b'} type. TODO
wfboxes = list(list(center=rep(0,ncol(x)), scale=rep(0,ncol(x)), col='gray', alpha=.2)), ##<< a
## list or a list of lists defining wire frame boxes to be plotted by the
## \code{'B'} type. TODO
planes = list(list(a=1, b=0, c=0, d=0, col='gray', alpha=.2)), ##<< a
## list or a list of lists defining planes to be plotted by the
## \code{'p'} type. TODO
texts = list(list(x=0, y=0, z=0, text = 'text', col = 'gray', alpha = .2)), ##<< a
## list or a list of lists defining texts to be plotted by the
## \code{'t'} type. TODO
debug = FALSE ##<< if TRUE, debugs will be printed. If numeric of value
## greater than 1, verbose debugs will be produced.
) {
  #library(rgl) # open3d,mfrow3d,spheres3d,lines3d,text3d
  #library(e1071) # bincombinations
  #library(geometry) # convhulln
  #library(tsiMisc) # vectorprod

  if (!is.matrix(x)) x<-as.matrix(x)
  if (nrow(x)==0) stop('\'x\' is empty')
  if (!is.null(cls)) cls<-as.factor(cls)

  # check the \code{tx} argument
  if (is.null(tx)) {
    stop('need an \'tx\' argument')
  }
  x1Txed<-tx(x[1,,drop=FALSE])
  if (!inherits(x1Txed,'matrix')) {
    stop('invalid \'tx\' argument: it does not produce a matrix ',
      '(maybe you forgot to add \'drop=FALSE\' when indexing your matrix')
  }


  # consolidate decorative arguments
  if (is.list(boxes) && length(boxes)>0 && !is.list(boxes[[1]])) {
    boxes<-list(boxes)
  }
  if (is.list(wfboxes) && length(wfboxes)>0 && !is.list(wfboxes[[1]])) {
    wfboxes<-list(wfboxes)
  }
  if (is.list(ellipses) && length(ellipses)>0 && !is.list(ellipses[[1]])) {
    ellipses<-list(ellipses)
  }
  if (is.list(planes) && length(planes)>0 && !is.list(planes[[1]])) {
    planes<-list(planes)
  }
  if (is.list(texts) && length(texts)>0 && !is.list(texts[[1]])) {
    texts<-list(texts)
  }

  # dimensionality of \code{x}
  k0<-ncol(x)
  if (debug) .pn(k0)
  if (debug) .pn(k)

  if (k0<3) {
    stop('we need at least 3D data')
  }
  if (ncol(x1Txed)!=3) {
    stop('\'tx\' must result in 3D data')
  }
  xTxed<-tx(x)

  if (!is.null(dimToShow)) {
    if (!is.null(k)) {
      stop('can\'t specify both the \'dimToShow\' and \'k\' arguments')
    }
    if (debug) {
      cat('guessing which dimensions to show from \'dimToShow\' argument\n')
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
    k<-sum(dimVisible)
    if (k<3) {
      stop('\'dimToShow\' argument selects ',sum(dimVisible),
        ' dimension(s), and we need at least 3 for a 3D plot')
    }
  } else if (!is.null(k) && k<k0) {
    if (k<3) {
      stop('invalid \'k\' argument, must be >= 3')
    }
    # user requested to draw fewer than \code{k0} dimensions
    # determine which dimensions to omit
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
      stop('can\'t determine which dimensions to show: ',
        '\'tx\' does not contain \'varExplained\' function')
    }
  } else {
    k<-k0
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
    if (!is.matrix(x2)) x2<-as.matrix(x2)
    if (nrow(x2)==0) stop('\'x2\' is empty')
    if (is.null(col2) || length(col2)==0) {
      if (!is.null(cls2) && !is.null(palette)) {
        col2<-palette[cls2]
      } else {
        col2<-'black'
      }
    }
  }
  if (!is.null(x3)) {
    if (!is.matrix(x3)) x3<-as.matrix(x3)
    if (nrow(x3)==0) stop('\'x3\' is empty')
    if (is.null(col3) || length(col3)==0) {
      if (!is.null(cls3) && !is.null(palette)) {
        col3<-palette[cls3]
      } else {
        col3<-'black'
      }
    }
  }

  xScaled<-scaleToUnit(x,-1,1)
  scalingTx<-attr(xScaled,'tx')
  scalingTxInv<-attr(xScaled,'txInv')
  scalingInvFactor<-mean(scalingTxInv(to.matrix(rep(1,k0)))-scalingTxInv(to.matrix(rep(0,k0))))
  if (debug>1) .pn(scalingInvFactor)
  if (debug>1) .pn(scalingTxInv(rbind(rep(-1,k0))))
  if (debug>1) .pn(scalingTxInv(rbind(rep(1,k0))))

  doPlotWireFrame<-stringr::str_detect(type,'w')
  doPlotScatters<-stringr::str_detect(type,'s')

  buildWireFrame<-function() {
    wireFrame<-matrix(NA,nrow=3*k*2^(k-1),ncol=k0)
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
          tmp<-rbind(v1,v2)
          colnames(tmp)<-colnames(x)
          wireFrame[cnt+1,]<-tmp[1,,drop=FALSE]
          wireFrame[cnt+2,]<-tmp[2,,drop=FALSE]
          # reserve lines of NA's
          cnt<-cnt+3
          #lines3d(rbind(tx(rbind(v1)),tx(v2)),color=c('gray'))
        }
        #lines3d(rbind(tx(v1),tx(rbind(v2))),color=c('gray','blue')[(in.convhull(v1)&in.convhull(v2))+1])
      }
    }
    if (debug>2) .pn(head(wireFrame))
    if (debug>3) .pn(wireFrame)
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
    colnames(vs)<-colnames(x)
    if (debug>1) .pn(head(vs))
    if (debug>3) .pn(vs)
    # convert from the range -1...1 (in k-dim space) to
    # the k-dim feature space of 'x'
    vsTxed<-scalingTxInv(rbind(vs))
    # convert from the k-dim feature space of 'x' to 3D
    vsTxed<-tx(vsTxed)
    # convert from the range -1...1 (in k-dim space) to 3D space
    vsUnitaryTxed<-tx(vs)
    # find convex hull - only points on the convex hull will be visible
    idx.convhull<-unique(as.numeric(geometry::convhulln(vsTxed)))
    #sort(idx.convhull)
    idxInConvhull<-1:nrow(vs)%in%idx.convhull
    signatureInConvhull<-rep(FALSE,2^k0)
    signatureInConvhull[bSignatures[idxInConvhull]]<-TRUE
    if (debug>2) .pn(head(signatureInConvhull))
    if (debug>3) .pn(signatureInConvhull)
  }
  if (doPlotWireFrame || str_detect(type,'B')) {
    wf<-buildWireFrame()
    if (debug>1) .pn(head(wf))
    if (debug>3) .pn(wf)
    # transform from the -1...1 in k-dim space to
    # the k-dim feature space of 'x'
    wfTxed<-scalingTxInv(wf)
    if (debug>2) .pn(head(wfTxed))
    if (debug>3) .pn(wfTxed)
    # transform the k-dim feature space of 'x' to 3D
    wfTxed<-tx(wfTxed)
    if (debug>2) .pn(head(wfTxed))
    if (debug>3) .pn(wfTxed)
  }

  ###
  ## plotting functions
  plotAxes<-function() {
    if (!is.null(colnames(x))) {
      v0<-rep(0,k0)
      axes.radius<-scalingInvFactor*c(.01,.01,.03,0)
      for (i in 1:k) {
        v1<-v2<-v3<-v4<-rep(0,k0)
        v1[dimVisibleIdx[i]]<-.85
        v2[dimVisibleIdx[i]]<-.85
        v3[dimVisibleIdx[i]]<-.9
        v4[dimVisibleIdx[i]]<-1
        tmp<-rbind(v0,v1,v2,v3,v4)
        colnames(tmp)<-colnames(x)
        tmp<-axesExpansion*tmp
        tmp<-tx(scalingTxInv(tmp))
        shade3d(cylinder3d(tmp[1:4,],
          radius=axes.radius,closed=-2),col=col.axes)
          #,emission=col.axes,specular=col.axes,alpha=1,shininess=0)
        text3d(tmp[5,,drop=FALSE],
          texts=colnames(x)[dimVisibleIdx[i]],col=col.axes)
          #,emission=col.axes,specular=col.axes,alpha=1,shinness=0)
      }
    }
  }

  # plot wire frame
  plotWireFrame<-function(annotate=FALSE) {
    #rgl.linestrips(wfTxed,color='lightgray',alpha=.5)
    lines3d(wfTxed,color='lightgray',alpha=.5)
    # the latter causes problems with writeWebGL in rgl v.0.95.1367
    #lines3d(wfTxed,color='lightgray',emission='lightgray',
    #specular='lightgray',ambient='lightgray',alpha=.5,shiness=50)

    if (annotate) {
      # mark point (-1,-1,-1,...,-1)
      v<-rep(.5,k0)
      v[dimVisibleIdx]<--1
      tmp<-rbind(v)
      colnames(tmp)<-colnames(x)
      tmp<-tx(scalingTxInv(tmp))
      points3d(tmp,color='black',size=10)
      # mark axes pointing away from (-1,-1,-1,...,-1,0,0,0)
      for (i in 1:k) {
        v<-rep(.5,k0)
        v[dimVisibleIdx]<--1
        v[dimVisibleIdx[i]]<-1
        tmp<-rbind(v)
        colnames(tmp)<-colnames(x)
        tmp<-tx(scalingTxInv(tmp))
        text3d(tmp,texts=i)
      }
    }
  }

  # plot 3D scatter plot of observations
  plot3dScatter<-function(x,size,col,alpha) {
    #spheres3d(tx(x),radius=radius,color=col,alpha=alpha)
    if (length(unique(size))==1) {
      points3d(xTxed,size=size[1],color=col,alpha=alpha)
    } else {
      tmp.size<-rep(size,length=nrow(x))
      tmp.col<-rep(col,length=nrow(x))
      tmp.alpha<-rep(alpha,length=nrow(x))
      for (s in unique(size)) {
        points3d(xTxed[tmp.size==s,,drop=FALSE],size=s,
          color=tmp.col[tmp.size==s],alpha=tmp.alpha[tmp.size==s])
      }
    }
  }

  # plot a projection of k-dimensional parallelepipedon having scatter
  # plots on its faces
  plotScatters<-function(x,size,col,alpha) {
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
                # now take vectors in the 3D transformed space
                v1txed<-vsTxed[i1,,drop=FALSE]
                v2txed<-vsTxed[i2,,drop=FALSE]
                v3txed<-vsTxed[i3,,drop=FALSE]
                v4txed<-vsTxed[i4,,drop=FALSE]
                # order the vertices such that the face points towards inside
                if (tcrossprod(vectorprod(
                  vsUnitaryTxed[i1,,drop=FALSE],
                  vsUnitaryTxed[i2,,drop=FALSE]),
                  vsUnitaryTxed[i4,,drop=FALSE])<0) {
                  w1<-v1txed
                  w2<-v2txed
                  w4<-v4txed
                  w3<-v3txed
                } else {
                  w1<-v1txed
                  w2<-v3txed
                  w4<-v4txed
                  w3<-v2txed
                }
                # create a scatter plot texture 
                png(textureFileName)
                opar<-par(mar=c(6,5,4,2)+.1, ask=FALSE)
                idx1<-which(w2-w1!=0)
                idx2<-which(w3-w1!=0)
                plot(x[,c(idx1,idx2)],pch=19,frame=F,col=alpha(col,alpha),cex=size/3,cex.lab=3)
                #,xlim=c(-1,1),ylim=c(-1,1))
                #text(0,.5,paste(idx1,idx2),cex=8)
                #text(0,-.5,paste(crossprod(vectorprod(v1,v2),v4)<0,
                #  paste(w2-w1,collapse=' '),paste(w3-w1,collapse=' '),sep=','),cex=3)
                par(opar)
                dev.off()
                # plot the texture
                #rgl.quads(tx(rbind(w1,w2,w4,w3)),texture=textureFileName,alpha=.5,
                #  texcoord=rbind(c(0,0),c(1,0),c(1,1),c(0,1)),lit=FALSE,front='fill',back='cull')
                tmp<-rbind(w1,w2,w4,w3)
                rgl.quads(tmp,
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

  nameListElements<-function(l,nms) {
    nm<-names(l)
    if (is.null(nm)) {
      nm<-vector('character',length(l))
    }
    nms<-nms[!nms%in%nm]
    for (i in 1:length(l)) {
      if (nchar(nm[i])==0 && length(nms)>0) {
        nm[i]<-nms[1]
        nms<-nms[-1]
      }
    }
    names(l)<-nm
    return(l)
  }
  # ex: nameListElements(list(1,2),c('a','b'))
  # ex: nameListElements(list(1,b=2,3),c('a','b','c'))

  # from a covariance matrix in the k0-dim features space compute
  # the covariance matrix in the transformed 3D space
  computeCovTxed<-function(cv) {
    # we have \code{cv = x' * x} for some \code{x} in \code{k0}-dim space,
    # but we need \code{cv2 = (tx(x))' * tx(x)} in 3D space.
    # As a first step, let's consider that \code{tx(x) = M*x}.
    # We can restore \code{x} by Cholesky decomposition
    # of the covariance matrix \code{cv}, and can compute \code{cv2}
    # directly by \code{t(M*x)*(M*x)}.
    # However, because \code{tx(x)} possibly involves centering,
    # such that \code{tx(x) = M*(x-x0) = M*x - M*x0}, we
    # need to cancel the effect of centering and compute \code{M*x} as
    # \code{M*x = tx(x) - tx(0)} because
    # \code{tx(x) - tx(0) = M*(x-x0) - M*(0-x0) = M*x - M*x0 +M*x0 = M*x}
    x<-chol(cv)
    if (debug>2) .pn(x)
    mx<-tx(x)-matrix(tx(to.matrix(rep(0,k0))),nrow=k0,ncol=3,byrow=TRUE)
    cvTxed<-crossprod(mx)
    return(cvTxed)
  }

  plotBoxes<-function(boxes) {
    if (debug) cat('-- plotting boxes\n')
    lapply(boxes,function(b) {
      knownNames<-c('center','scale','col','alpha')
      if (debug>1) .pn(b)
      b<-nameListElements(b,knownNames)
      if (debug>2) .pn(b)

      if (!is.null(b$scale)) {
        scl<-rep(b$scale,length.out=k0)
      } else {
        scl<-rep(1,k0)
      }
      if (debug>2) .pn(scl)
      # transform \code{scl} without possibly centering first:
      # consider \code{tx} performs \code{tx(scl) = M*(scl-center)},
      # but we need \code{M*scl}.
      # Because \code{M*(scl-center) = M*scl - M*center}
      # and \code{M*(0-center) = -M*center}
      # \code{M*scl} can be computed as
      # \code{M*scl = M*(scl-center) + M*center = M*(scl-center) - M*(0-center) = tx(scl) - tx(0)}
      scl<-tx(to.matrix(scl))-tx(to.matrix(rep(0,k0)))
      if (debug>2) .pn(scl)
      if (!is.null(b$center)) {
        center<-rep(b$center,length.out=k0)
      } else {
        center<-rep(0,k0)
      }
      center<-tx(to.matrix(center))
      if (debug>2) .pn(center)

      col<-ifelse(!is.null(b$col),b$col,'gray')
      alpha<-ifelse(!is.null(b$alpha),b$alpha,.2)
      tmp<-cube3d(col=col,alpha=alpha)
      tmp<-scale3d(tmp,scl[1],scl[2],scl[3])
      tmp<-translate3d(tmp,center[1],center[2],center[3])
      do.call('shade3d',c(list(x=tmp),b[!names(b)%in%knownNames]))
    })
  }

  plotWireFrameBoxes<-function(boxes) {
    if (debug) cat('-- plotting wire frame boxes\n')
    lapply(boxes,function(b) {
      knownNames<-c('center','scale','col','alpha')
      if (debug>1) .pn(b)
      b<-nameListElements(b,knownNames)
      if (debug>2) .pn(b)

      if (!is.null(b$scale)) {
        scl<-rep(b$scale,length.out=k0)
      } else {
        scl<-rep(1,k0)
      }
      if (debug>2) .pn(scl)
      # transform \code{scl} without possibly centering first:
      # consider \code{tx} performs \code{tx(scl) = M*(scl-center)},
      # but we need \code{M*scl}.
      # Because \code{M*(scl-center) = M*scl - M*center}
      # and \code{M*(0-center) = -M*center}
      # \code{M*scl} can be computed as
      # \code{M*scl = M*(scl-center) + M*center = M*(scl-center) - M*(0-center) = tx(scl) - tx(0)}
      sclTxed<-tx(to.matrix(scl))-tx(to.matrix(rep(0,k0)))
      if (debug>2) .pn(sclTxed)
      if (!is.null(b$center)) {
        center<-rep(b$center,length.out=k0)
      } else {
        center<-rep(0,k0)
      }
      if (debug>2) .pn(center)
      centerTxed<-tx(to.matrix(center))
      if (debug>2) .pn(centerTxed)

      wfBox<-wf
      for (i in 1:k0) {
        wfBox[,i]<-wfBox[,i]*scl[i]/2+center[i]
      }
      if (debug>2) .pn(head(wfBox))
      if (debug>3) .pn(wfBox)
      # transform the k-dim feature space of 'x' to 3D
      wfBoxTxed<-tx(wfBox)
      if (debug>2) .pn(head(wfBoxTxed))
      if (debug>3) .pn(wfBoxTxed)

      col<-ifelse(!is.null(b$col),b$col,'gray')
      alpha<-ifelse(!is.null(b$alpha),b$alpha,.2)
      do.call('lines3d',c(list(x=wfBoxTxed,col=col,alpha=alpha),b[!names(b)%in%knownNames]))
    })
  }

  plotEllipses<-function(ellipses) {
    if (debug) cat('-- plotting ellipses\n')
    lapply(ellipses,function(e) {
      knownNames<-c('center','cov','col','alpha')
      if (debug>1) .pn(e)
      e<-nameListElements(e,knownNames)
      if (debug>2) .pn(e)

      if (!is.null(e$center)) {
        center<-e$center
      } else {
        center<-rep(0,k0)
      }
      center<-tx(to.matrix(center))
      if (debug>2) .pn(center)

      if (!is.null(e$cov)) {
        cv<-e$cov
      } else {
        cv<-diag(rep(1,k0))
      }
      if (debug>2) .pn(cv)
      cvTxed<-computeCovTxed(cv)
      if (debug>2) .pn(cvTxed)
      col<-ifelse(!is.null(e$col),e$col,'gray')
      alpha<-ifelse(!is.null(e$alpha),e$alpha,.2)
      tmp<-ellipse3d(cvTxed,centre=center)
      do.call('plot3d',c(list(x=tmp,col=col,alpha=alpha,add=TRUE),e[!names(e)%in%knownNames]))
    })
  }

  plotPlanes<-function(planes) {
    if (debug) cat('-- plotting planes\n')
    lapply(planes,function(p) {
      if (debug>1) .pn(p)
      do.call('planes3d',p)
    })
  }

  plotTexts<-function(texts) {
    if (debug) cat('-- plotting texts\n')
    lapply(texts,function(t) {
      if (debug>1) .pn(t)
      knownNames<-c('x','text')
      t<-nameListElements(t,knownNames)
      if (debug>2) .pn(t)
      if (is.null(t$x)) stop('\'x\' element missing from \'texts\'')
      x<-tx(to.matrix(t$x))
      if (is.null(t$text)) stop('\'text\' element missing from \'texts\'')
      do.call('text3d',c(list(x=x[1],y=x[2],z=x[3],text=t$text),
        t[!names(t)%in%c('x','text')]))
    })
  }

  if (debug) .pn(devices)
  scenes<-c()
  # split to scenes
  sceneTypes<-str_split(type,'\\|')[[1]]
  for (i in seq(along=sceneTypes)) {
    sceneType<-sceneTypes[i]

    if (debug) cat(sprintf('plotting scene "%s"\n',sceneType));
    if (length(devices)>=i && devices[i]%in%rgl.dev.list()) {
      if (debug) cat(sprintf('reusing device %d for scene %d\n',devices[i],i));
      rgl.set(devices[i])#,silent=TRUE
      rgl.clear(type='shapes')
      rgl.clear(type='userviewpoint')
    } else {
      if (debug) cat('opening a new device\n')
      open3d()
    }
    scenes<-c(scenes,rgl.cur())
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
    if (debug) {
      .pn(length(subsceneRowTypes))
      .pn(maxRowSize)
    }
    m<-matrix(1:(maxRowSize*length(subsceneRowTypes)),
      length(subsceneRowTypes),maxRowSize,byrow=TRUE)
    widths<-rep(widths[1:min(length(widths),maxRowSize)],
      length.out=maxRowSize)
    heights<-rep(heights[1:min(length(heights),length(subsceneRowTypes))],
      length.out=length(subsceneRowTypes))
    layout3d(mat=m,widths=widths,heights=heights,sharedMouse=TRUE)
    if (debug) {
      .pn(m)
      .pn(widths)
      .pn(heights)
    }
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
        decorate<-FALSE
        plotTitle<-FALSE
        plotTypes<-str_split(subsceneType,'')[[1]]
        plotTypeIdx<-1
        while (plotTypeIdx<=str_length(subsceneType)) {
          plotType<-plotTypes[plotTypeIdx]
          plotTypeIdx<-plotTypeIdx+1
          # is there a numer following the plotType?
          if (plotTypeIdx<=str_length(subsceneType) &&
            regexpr('[1-3]',plotTypes[plotTypeIdx])!=-1) {
              xi<-as.integer(plotTypes[plotTypeIdx])
              plotTypeIdx<-plotTypeIdx+1
              switch(xi,
                , # no need to test 'x', as it must always be present
                if (is.null(x2)) {
                  stop(paste0('unspecified \'x2\' data referred to from subscene \'',
                    subsceneType,'\''))
                },
                if (is.null(x3)) {
                  stop(paste0('unspecified \'x3\' data referred to from subscene \'',
                    subsceneType,'\''))
                },
                stop(paste0('invalid data referred to from plotTypes \'',
                    plotTypes,'\''))
              )
          } else {
            xi<-1
          }
          if (debug) cat(sprintf('    seen plotType "%s"\n',plotType));
          if (length(plotType)>0) {
            switch(plotType,
              'a'=plotAxes(),
              'b'=plotBoxes(boxes),
              'B'=plotWireFrameBoxes(wfboxes),
              'd'=decorate<-TRUE,
              'e'=plotEllipses(ellipses),
              'f'=switch(xi,
                plotScatters(x,size,col,alpha),
                plotScatters(x2,size2,col2,alpha2),
                plotScatters(x3,size3,col3,alpha3)),
              'm'=plotTitle<-TRUE,
              'p'=plotPlanes(planes),
              's'=switch(xi,
                plot3dScatter(x,size,col,alpha),
                plot3dScatter(x2,size2,col2,alpha2),
                plot3dScatter(x3,size3,col3,alpha3)),
              't'=plotTexts(texts),
              'w'=plotWireFrame(annotateWireFrame),
              'otherwise'=stop('unknown type "',plotType,'"'))
          }
        }
        if (decorate || plotTitle) {
          if (plotTitle) {
            mn<-switch(xi,
                main,
                main2,
                main3,
                default=stop())
          } else {
            mn<-NULL
          }
          if (decorate) {
            xlab<-colnames(xTxed)[1]
            ylab<-colnames(xTxed)[2]
            zlab<-colnames(xTxed)[3]
          } else {
            xlab<-ylab<-zlab<-NULL
          }
          decorate3d(
            xlab=xlab,ylab=ylab,zlab=zlab,
            box=decorate,axes=decorate,
            main=mn)
        }
      }
    }
  }
  rglSetMouseCbTrackball(scenes)

  return(scenes)
  ### a list of rgl device IDs holding the scenes plotted

},ex=function() {
  if (interactive() && require(rgl)) {
    # Plot a 3D scatter plot and 2D scatter plots as
    # faces of a parallelepipedon:
    plot3dProj(iris[,1:3], cls=iris$Species)

    # Plot 4D data in a 3D scatter plot on a new device,
    # use PCA to go from 4D to 3D, and add a bounding box and 3D axes
    # decoration (\code{type='d'}) on top of the axes of the 4D space
    # (\code{type='a'}).
    plot3dProj(iris[,1:4], cls=iris$Species, tx=txPca(iris[,1:4]), type='sad', devices=NULL)

    # Plot the iris data set with additional decoration: ellipses
    # representing individual species of the flowers, a box and a text
    # in the center, and a plane.
    iris.setosa<-iris[iris$Species=='setosa',1:3]
    iris.versicolor<-iris[iris$Species=='versicolor',1:3]
    iris.virginica<-iris[iris$Species=='virginica',1:3]
    plot3dProj(iris[,1:3], col=c('red','green','blue')[as.numeric(iris$Species)],
      type='sdpbet', # scatter plot with decoration, a plane, a box,
                     # elippses and a text
      ellipse=list(
        list(cov(iris.setosa),center=colMeans(iris.setosa),col='red',alpha=.1),
        list(cov(iris.versicolor),center=colMeans(iris.versicolor),col='green',alpha=.1),
        list(cov(iris.virginica),center=colMeans(iris.virginica),col='blue',alpha=.1)),
      box=list(center=colMeans(iris[,1:3]),scale=c(.3,.3,.3),col='black',alpha=.2),
      planes=list(0,0,1,-2.5,col='yellow',alpha=.2),
      texts=list(colMeans(iris[,1:3]),text='center',col='black'),
      devices=NULL)

    # Plot two data sets: all Iris flowers (on the left side) and
    # Setosa and Versicolor species only (on the right side).
    # In both cases, produce 2D and 3D spatter plots.
    # For the second data set, alter the size and the alpha value
    # of Setosa flowers. Also, plot small axes under the scatter plots.
    plot3dProj(x = iris[, 1:3], cls = iris$Species,
      main = 'Setosa, Versicolor and Virginica',
      x2 = iris[iris$Species != 'virginica', 1:3],
      cls2 = iris$Species[iris$Species != 'virginica'],
      alpha2 = c(1,.3)[1+(iris$Species[iris$Species != 'virginica']=='setosa')],
      size2 = c(3,10)[1+(iris$Species[iris$Species != 'virginica']=='setosa')],
      main2 = 'Setosa and Versicolor',
      ty='fw,swm,s2m2d,f2;,a',heights=c(2,1),
      devices=NULL) # devices=NULL opens a new 'rgl' device
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
