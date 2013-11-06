\name{fitted.jags_analysis}
\alias{fitted.jags_analysis}
\title{JAGS analysis fitted values}
\usage{
  \method{fitted}{jags_analysis} (object,
    parm = "prediction", model_number = 1,
    derived_code = NULL, random_effects = NULL,
    level = "current", estimate = "current", data = NULL,
    ...)
}
\arguments{
  \item{object}{a jags_analysis object.}

  \item{parm}{a character element naming the derived
  parameter for which the estimates should be calculated
  (default = "prediction").}

  \item{model_number}{an integer vector specifying the
  model to select. If model_number = 0 then it selects the
  model with the lowest DIC.}

  \item{derived_code}{a character element defining a block
  in the JAGS dialect of the BUGS language that defines one
  or more derived ' parameters for each row of data. If
  NULL derived_code is as defined by the JAGS model for
  which the JAGS analysis was performed.}

  \item{random_effects}{a named list which specifies which
  parameters to treat as random variables. If NULL random
  is as defined by the JAGS model for which the JAGS
  analysis was performed.}

  \item{level}{a numeric scalar specifying the significance
  level or a character scalar specifying which mode the
  level should be taken from. By default the level is as
  currently specified by \code{opts_jagr} in the global
  options.}

  \item{ndata}{the dataset for which to calculate the
  residuals. By default data is NULL as the residuals are
  typically calculated on the original dataset.}

  \item{...}{further arguments passed to or from other
  methods.}
}
\value{
  the original data frame with the median estimate and
  credibility intervals for the derived parameter of
  interest
}
\description{
  Calculate fitted values with credible limits for a JAGS
  analysis
}
\seealso{
  \code{\link{jags_model}}, \code{\link{jags_analysis}} and
  \code{\link{predict.jags_analysis}}
}

