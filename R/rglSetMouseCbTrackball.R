rglSetMouseCbTrackball<-structure(
function # Link the current device with others to share mouse control using the 'trackball' mode.
##description<<
## 'rglSetMouseCbTrackball' sets mouse "trackball" callback for given
## button on selected device(s), such that interaction made using the
## given button affects all the devices (and all their subscenes, by
## default).
##
## The code is mostly based on the 'mouseCallbacks' demo from the
## 'rgl' package.
(dev = rgl.cur(), ##<< device(s) to set callback for
button = 1, ##<< button to set callback for
affectSubscenes = TRUE ##<< if TRUE, interaction affects all subscenes
## in a device, not only the current one
) {
  if (require(rgl)) {
    width <- height <- rotBase <- NULL
    userMatrix <- list()
    cur <- rgl.cur()

    vlen <- function(a) sqrt(sum(a^2))

    angle <- function(a,b) {
      dot <- sum(a*b)
      acos(dot/vlen(a)/vlen(b))
    }


    screenToVector <- function(x, y) {
      radius <- max(width, height)/2
      centre <- c(width, height)/2
      pt <- (c(x, y) - centre)/radius
      len <- vlen(pt)

      if (len > 1.e-6) pt <- pt/len

      maxlen <- sqrt(2)
      angle <- (maxlen - len)/maxlen*pi/2
      z <- sin(angle)
      len <- sqrt(1 - z^2)
      pt <- pt * len
      return (c(pt, z))
    }
  
    trackballBegin <- function(x, y) {
      vp <- par3d("viewport")
      width <<- vp[3]
      height <<- vp[4]
      cur <<- rgl.cur()
      for (i in dev) {
        if (inherits(try(rgl.set(i, TRUE)), "try-error")) dev <<- dev[dev != i]
        else userMatrix[[i]] <<- par3d("userMatrix")
      }
      rgl.set(cur, TRUE)
      rotBase <<- screenToVector(x, height - y)
    }
  
    trackballUpdate <- function(x,y) {
      rotCurrent <- screenToVector(x, height - y)
      angle <- angle(rotBase, rotCurrent)
      axis <- vectorprod(rotBase, rotCurrent)
      mouseMatrix <- rotationMatrix(angle, axis[1], axis[2], axis[3])
      for (i in dev) {
        if (inherits(try(rgl.set(i, TRUE)), "try-error")) {
          dev <<- dev[dev != i]
        } else {
          subscenes <- subsceneList(window=i)
          if (is.null(subscenes)) {
            subscenes <- currentSubscene3d()
          }
          for (s in subscenes) {
            par3d(userMatrix = mouseMatrix %*% userMatrix[[i]], subscene = s)
          }
        }
      }
      rgl.set(cur, TRUE)
    }
  
    for (i in dev) {
      rgl.set(i, TRUE)
      rgl.setMouseCallbacks(button, begin = trackballBegin, update = trackballUpdate, end = NULL)
    }
    rgl.set(cur, TRUE)
  }
},ex=function() {
  if (interactive() && require(rgl)) {
    dev1 <- open3d()
    shade3d(cube3d(color = rep(rainbow(6), rep(4, 6))))
    dev2 <- open3d()
    mfrow3d(1, 2, sharedMouse = TRUE)
    shade3d(cube3d(color = rep(rainbow(6), rep(4, 6))))
    next3d()
    shade3d(cube3d(color = rep(rainbow(6), rep(4, 6))))
    rglSetMouseCbTrackball(c(dev1, dev2))
  }
})
