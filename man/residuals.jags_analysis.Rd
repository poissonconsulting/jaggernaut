\name{residuals.jags_analysis}
\alias{residuals.jags_analysis}
\title{Residuals}
\usage{
  \method{residuals}{jags_analysis} (object,
    parm = "residual", model_number = 1,
    derived_code = NULL, random_effects = NULL,
    level = "current", estimate = "current", data = NULL,
    ...)
}
\arguments{
  \item{object}{a jags_analysis object.}

  \item{parm}{a character element naming the derived
  parameter for which the estimates should be calculated by
  default = "residual"}

  \item{model_number}{an integer vector specifying the
  model to select. If model_number = 0 then it selects the
  model with the lowest DIC.}

  \item{derived_code}{a character element defining a block
  in the JAGS dialect of the BUGS language that defines one
  or more derived parameters for each row of data. If NULL
  derived_code is as defined by the JAGS model for which
  the JAGS analysis was performed.}

  \item{random_effects}{a named list which specifies which
  parameters to treat as random variables. If NULL random
  is as defined by the JAGS model for which the JAGS
  analysis was performed.}

  \item{level}{a numeric scalar specifying the significance
  level or a character scalar specifying which mode the
  level should be taken from. By default the level is as
  currently specified by \code{opts_jagr} in the global
  options.}

  \item{estimate}{a character scalar specifying whether the
  point estimate should be the "mean" or the "median" or a
  character scalar which mode the level should be taken
  from. By default the estimate is as currently specified
  by \code{opts_jagr} in the global options.}

  \item{data}{the dataset for which to calculate the
  residuals. By default newdata is NULL as the residuals
  are typically calculated on the original dataset.}

  \item{...}{further arguments passed to or from other
  methods.}
}
\value{
  the input data frame with the point estimate and
  credibility intervals for the residuals
}
\description{
  Get residuals with estimates using derived code in a JAGS
  analysis
}
\seealso{
  \code{\link{jags_model}} and \code{\link{jags_analysis}}
}

