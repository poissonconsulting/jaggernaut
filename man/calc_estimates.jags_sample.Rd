\name{calc_estimates.jags_sample}
\alias{calc_estimates.jags_sample}
\title{Calculate estimates}
\usage{
  \method{calc_estimates}{jags_sample} (object,
    level = "current")
}
\arguments{
  \item{object}{a data.frame or list of data.frames from
  predict(...,level = "no")}

  \item{level}{a numeric scalar specifying the significance
  level or a character scalar specifying which mode the
  level should be taken from. By default the level is as
  currently specified by \code{opts_jagr} in the global
  options.}
}
\value{
  a data frame with the median estimates and credibility
  intervals for the derived parameter of interest
}
\description{
  Calculates estimates for a jags_sample object
}
\seealso{
  \code{\link{predict.jags_analysis}}
}

