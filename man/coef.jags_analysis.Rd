\name{coef.jags_analysis}
\alias{coef.jags_analysis}
\title{Calculate parameter estimates}
\usage{
  \method{coef}{jags_analysis} (object, parm = "fixed",
    level = "current", estimate = "current", ...)
}
\arguments{
  \item{object}{a \code{jags_analysis} object}

  \item{parm}{a character vector of the parameters to
  calculate the estimates}

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
  estimate and lower and upper credible limits as well as
  the standard deviation, percent relative error and
  significance
}
\description{
  Calculates parameter estimates for a JAGS analysis
}
\seealso{
  \code{\link{jags_analysis}}, \code{\link{opts_jagr}} and
  \code{\link{jaggernaut}}
}

