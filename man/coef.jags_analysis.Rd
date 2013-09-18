\name{coef.jags_analysis}
\alias{coef.jags_analysis}
\title{Calculate parameter estimates}
\usage{
  \method{coef}{jags_analysis} (object, parm = "fixed",
    level = "current", ...)
}
\arguments{
  \item{object}{a \code{jags_analysis} object}

  \item{model_number}{an integer element specifying the
  model to select. If model_number = 0 then it selects the
  model with the lowest DIC.}

  \item{parm}{a character vector of the parameters to
  calculate the estimates}

  \item{as_list}{a logical element specifying whether to
  return the coef as as list or a data.frame (the
  default).}

  \item{level}{a numeric scalar specifying the significance
  level or a character scalar specifying which mode the
  level should be taken from. By default the level is as
  currently specified by \code{opts_jagr} in the global
  options.}

  \item{...}{further arguments passed to or from other
  methods.}
}
\value{
  a data.frame of the parameter estimates with the median
  estimate and lower and upper credible limits as well as
  the percent relative error and significance
}
\description{
  Calculates parameter estimates for a JAGS analysis
}
\seealso{
  \code{\link{jags_analysis}} and \code{\link{jaggernaut}}
}

