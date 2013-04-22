\name{plot.jags_analysis}
\alias{plot.jags_analysis}
\title{Plot a JAGS analysis}
\usage{
  \method{plot}{jags_analysis} (x, model = 1, ...)
}
\arguments{
  \item{x}{a jags_analysis object}

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
  \code{\link{jags_analysis}} and
  \code{\link[coda]{plot.mcmc}}
}

