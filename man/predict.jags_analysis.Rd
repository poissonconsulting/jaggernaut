\name{predict.jags_analysis}
\alias{predict.jags_analysis}
\title{JAGS analysis predictions}
\usage{
  \method{predict}{jags_analysis} (object, newdata = NULL,
    parm = "prediction", base = FALSE, values = NULL,
    model_number = 1, derived_code = NULL,
    random_effects = NULL, level = "current",
    length_out = 50, ...)
}
\arguments{
  \item{object}{a jags_analysis object.}

  \item{newdata}{a data.frame of the data values over which
  to calculate the estimates of the derived parameter or a
  character vector specify the variable or variable
  combination for which to calculate the estimates of the
  derived parameter. If NULL (the default) the derived
  parameter is calculated for each row in the original data
  set.}

  \item{parm}{a character element naming the derived
  parameter for which the estimates should be calculated.}

  \item{base}{a boolean element indicating whether or not
  to express the expected value as a percent change of a
  base level or a data frame defining the base level.}

  \item{values}{a data frame with a single row that defines
  the value of particular variables. The variables in the
  arguments data and base are replaced by the corresponding
  values.}

  \item{model_number}{an integer vector specifying the
  model to select. If model_number = 0 then it selects the
  model with the lowest DIC.}

  \item{derived_code}{a character element defining a block
  in the JAGS dialect of the BUGS language that defines one
  or more derived parameters for each row of data. If NULL
  derived_code is as defined by the JAGS model for which
  the JAGS analysis was performed.}

  \item{random_effects}{a named list which specifies which
  parameters to treat as random variables in the derived
  code. If NULL random_effects is as defined by the JAGS
  model for which the JAGS analysis was performed.}

  \item{level}{a numeric scalar specifying the significance
  level or a character scalar specifying which mode the
  level should be taken from. By default the level is as
  currently specified by \code{opts_jagr} in the global
  options.}

  \item{length_out}{an integer element indicating the
  number of values when creating a sequence of values
  across the range of a continuous variable.}

  \item{...}{further arguments passed to or from other
  methods.}
}
\value{
  the input data frame with the median estimate and
  credibility intervals for the derived parameter of
  interest
}
\description{
  Calculate predictions with estimates for derived
  parameters in a JAGS analysis
}
\seealso{
  \code{\link{jags_model}}, \code{\link{jags_analysis}} and
  \code{\link{jaggernaut}}
}

