\name{coef.jags_analysis}
\alias{coef.jags_analysis}
\title{Calculate parameter estimates}
\usage{
  \method{coef}{jags_analysis} (object, model = 1,
    parm = "fixed", level = "current", ...)
}
\arguments{
  \item{object}{a \code{jags_analysis} object}

  \item{model}{an integer element specifying the model to
  select. If model = 0 then it selects the model with the
  lowest DIC.}

  \item{parm}{a character vector of the parameters to
  calculate the estimates}

  \item{level}{a numeric scalar specifying the significance
  level or a character scalar specifying which mode the
  level should be taken from. By default the level is as
  currently specified by \code{opts_jagr0} in the global
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

