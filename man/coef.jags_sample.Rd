\name{coef.jags_sample}
\alias{coef.jags_sample}
\title{Coefficients}
\usage{
\method{coef}{jags_sample}(object, level = "current", estimate = "current",
  ...)
}
\arguments{
  \item{object}{a jags_sample object from predict(...,level
  = "no")}

  \item{level}{a numeric scalar specifying the significance
  level or a character scalar specifying which mode the
  level should be taken from. By default the level is as
  currently specified by \code{opts_jagr} in the global
  options.}

  \item{estimate}{a character scalar specifying whether the
  point estimate should be the "mean" or the "median" or a
  character scalar which mode the level should be #' taken
  from. By default the estimate is as currently specified
  by \code{opts_jagr} in the global options.}

  \item{...}{further arguments passed to or from other
  methods.}
}
\value{
a data.frame of the parameter estimates with the point
estimate and lower and upper credible limits as well as the
standard deviation, percent relative error and significance
}
\description{
Calculates coefficients for a jags_sample object
}
\seealso{
\code{\link{opts_jagr}} and \code{\link{jaggernaut}}
}

