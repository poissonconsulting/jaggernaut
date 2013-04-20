\name{plot.janalysis}
\alias{plot.janalysis}
\title{Plot a JAGS analysis}
\usage{
  \method{plot}{janalysis} (x, model = 1, ...)
}
\arguments{
  \item{x}{a janalysis object}

  \item{model}{an integer element specifying the model to
  select. If model = 0 then it selects the model with the
  lowest DIC.}

  \item{...}{arguments to pass to coda::plot.mcmc}
}
\description{
  Plots the MCMC samples from a JAGS analysis model using
  the coda::plot.mcmc function.
}
\seealso{
  \code{\link{jaggernaut}}, \code{\link{analysis}} and
  \code{\link[coda]{plot.mcmc}}
}

