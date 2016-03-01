lnormPar<-structure(
function # Lognormal distribution parameter conversion.
##description<<
## 'lnormPar' converts the parameters of lognormal distribution
## between linear and log scales.
## Lognormal distribution is naturally characterized by parameters on
## the log scale. However, the distribution can also be parameterized
## on the linear scale, e.g. using sample-based parameters estimates.
## 'lnormPar' accepts either the 'mean' and 'sd' parameters
## from the linear scale and computes the 'meanlog' and 'sdlog'
## parameters on the log scale, or 'meanlog' and 'sdlog' parameters
## from the log scale and computes the 'mean' and 'sd' parameters on
## the linear scale.
##
##references<<
## http://www.mathworks.com/help/stats/lognstat.html
(mean = NULL, ##<< the mean on the linear scale
sd = NULL, ##<< the standard deviation on the linear scale
meanlog = NULL, ##<< the mean on the log scale
sdlog = NULL ##<< the standard deviation on the log scale
) {
  fail<-FALSE
  if (sum(c(!is.null(mean),!is.null(sd),!is.null(meanlog),!is.null(sdlog))) != 2) fail<-TRUE
  if (!fail) {
    if (!is.null(mean) && !is.null(sd)) {
      rv<-list(
        meanlog = log(mean/sqrt(1+(sd/mean)^2)),
        sdlog = sqrt(log(1+(sd/mean)^2)),
        mean = mean,
        sd = sd)
    } else if (!is.null(meanlog) && !is.null(sdlog)) {
      rv<-list(
        mean = exp(meanlog+(sdlog^2)/2),
        sd = sqrt((exp(sdlog^2)-1)*exp(2*meanlog+sdlog^2)),
        meanlog = meanlog,
        sdlog = sdlog
      )
    } else fail<-TRUE
  }
  if (fail) stop('either \'mean\' and \'sd\', or \'meanlog\' and \'sdlog\' expected')
  return(rv)
  ### a named list of length 4 holding both the computed parameters and
  ### the input paramaters. The parameter names correspond to the names
  ### of the arguments, i.e. 'mean','sd','meanlog','sdlog'. The
  ### computed parameters come first in the list.
},ex=function() {
  # convert parameters from the linear scale to the log scale
  paramsOnLogScale <- lnormPar(mean = 10, sd = 3)
  print(paramsOnLogScale)

  # convert parameters from the log scale to the linear scale
  paramsOnLinearScale <- lnormPar(meanlog = paramsOnLogScale$meanlog, sdlog = paramsOnLogScale$sdlog)
  print(paramsOnLinearScale)

  # Check the conversion empirically:
  # estimate the mean and standard deviation of a sample drawn from the
  # lognormal distribution with (log) parameters computed from
  # parameters on the linear scale
  x<-rlnorm(10000, paramsOnLogScale$meanlog, paramsOnLogScale$sdlog)
  print(mean(x))
  print(sd(x))
})
