movieSpin3d<-structure(
function # Create a movie by rotating a 3D rgl scene.
##<< details
## 'movieSpin3d' is basically just a wrapper for
## 'movie3d(spin3d(axis=axis, rpm = rpm), fps = fps, ...)'
## that sets the duration to the value of '60/rpm-1/fps', which
## results in a sequence of frames that form a continuous animation
## when repeated. This is in contrast to setting the duration to the
## value of 60/rpm, which results in rendering the last frame
## identical to the first one, thus making the animation non-smooth.
##<<sealso rgl::movie3d, rgl::play3d
(axis, ##<< axis to rotate around
rpm, ##<< the speed of rotation (rotations per minute)
fps = 5, ##<< frame rate (frames per second)
... ##<< additional arguments passed to movie3d
) {
  # 'rpm' rpm,
  # 'rpm'/60 rotation per second,
  # period of 60/'rpm',
  # 'fps' fps,
  # 'fps'*60/'rpm' frames, but this setting apparently
  # results in one more frame (such that the first and the last frame
  # in a moview are the same), so we need to shorten the duration to
  # 60/'rpm'
  # Example:
  # rpm=5, fps=10,
  # period=60/rpm=60/5=12,
  # frames=fps*period=fps*60/rpm=120,
  # duration=frames/fps=fps*60/rpm/fps=60/rpm=12,
  # updated duration=(fps*60/rpm-1)/fps=60/rpm-1/fps=12-1/10
  #
  # To slow rotation (e.g. half 'rpm') but keep the number of frames
  # constant, we need to half 'fps'. To slow rotation (e.g. half 'rpm')
  # but keep the 'fps' constant, we need to double the number of
  # frames.
  movie3d(spin3d(axis=axis,rpm=rpm),duration=60/rpm-1/fps,fps=fps,...)
},ex=function() {
  ##dontrun:
  #movieSpin3d(axis=c(1,1,1),rpm=5,fps=4)
})
