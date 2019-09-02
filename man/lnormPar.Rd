\name{lnormPar}
\alias{lnormPar}
\title{Lognormal distribution parameter conversion.}
\description{\code{\link{lnormPar}} converts the parameters of lognormal distribution
between linear and log scales.
Lognormal distribution is naturally characterized by parameters on
the log scale. However, the distribution can also be parameterized
on the linear scale, e.g. using sample-based parameters estimates.
\code{\link{lnormPar}} accepts either the \code{mean} and \code{sd}
parameters from the linear scale and computes the \code{meanlog} and
\code{sdlog} parameters on the log scale, or \code{meanlog} and
\code{sdlog} parameters from the log scale and computes the
\code{mean} and \code{sd} parameters on the linear scale.
}
\usage{lnormPar(mean = NULL, sd = NULL, meanlog = NULL, sdlog = NULL)}
\arguments{
  \item{mean}{the mean on the linear scale}
  \item{sd}{the standard deviation on the linear scale}
  \item{meanlog}{the mean on the log scale}
  \item{sdlog}{the standard deviation on the log scale}
}

\value{a named list of length 4 holding both the computed parameters and
the input paramaters. The parameter names correspond to the names
of the arguments, i.e. \code{mean}, \code{sd}, \code{meanlog}, and
\code{sdlog}. The computed parameters come first in the list.}
\references{\url{http://www.mathworks.com/help/stats/lognstat.html}}
\author{Tomas Sieger}




\examples{
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
}
