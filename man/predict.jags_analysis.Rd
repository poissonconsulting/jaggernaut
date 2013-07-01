\name{predict.jags_analysis}
\alias{predict.jags_analysis}
\title{JAGS analysis predictions}
\usage{
  \method{predict}{jags_analysis} (object, newdata = NULL,
    parm = "prediction", base = FALSE, values = NULL,
    model_number = 1, derived_code = NULL,
    random_effects = NULL, level = "current",
    length_out = 50, obs_by = NULL, split_by = NULL,
    unique_by = NULL, ...)
}
\arguments{
  \item{object}{a jags_analysis}

  \item{newdata}{a data.frame or data list of the data
  values over which to calculate the estimates of the
  derived parameter or a character vector specify the
  variable or variable combination for which to calculate
  the estimates of the derived parameter. If NULL (the
  default) the derived parameter is calculated for each row
  in the original data set.}

  \item{parm}{a character element naming the derived
  parameter for which the estimates should be calculated.}

  \item{base}{a boolean element indicating whether or not
  to express the expected value as a percent change of a
  base level or a data frame defining the base level.}

  \item{values}{a data frame with a single row that defines
  the value of particular variables. The variables in the
  arguments newdata and base are replaced by the
  corresponding values.}

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

  \item{obs_by}{a character vector indicating which factors
  to only predict for their observed factor combinations.}

  \item{split_by}{a character vector indicating which
  factors to split the predictions by.}

  \item{unique_by}{a character vector indicating which
  factors to unique by.}

  \item{...}{further arguments passed to or from other
  methods.}
}
\value{
  a data frame with the median estimates and credibility
  intervals for the derived parameter of interest or if
  level = "no" an object of class jags_samples
}
\description{
  Calculate predictions with estimates for derived
  parameters in a JAGS analysis or if level = "no" returns
  an object of class jags_samples
}
\details{
  Its important to realise that if the original data set
  was a data list then newdata must be a data list or NULL,
  base must be FALSE and values must be NULL. Otherwise if
  the original data set was a data frame then newdata can
  be NULL, a character vector of the variables to generate
  data for or a data.frame and base and values can be
  data.frames.

  Its also important to realize that values always replaces
  the corresponding values in base but only replaces the
  corresponding values in newdata if they are unaltered,
  i.e., as they are in dataset(object,base = TRUE).
}
\seealso{
  \code{\link{jags_model}}, \code{\link{jags_analysis}} and
  \code{\link{jaggernaut}}
}

