\name{plot.janalysis}
\alias{plot.janalysis}
\title{Plot a JAGS analysis}
\usage{
  \method{plot}{janalysis} (x, model = 1, ...)
}
\arguments{
  \item{x}{a janalysis object #'}

  \item{model}{an integer vector specifying the model to
  select. If model = 0 then it selects the model with the
  lowest DIC.}

  \item{...}{arguments to pass to coda::plot.mcmc}
}
\description{
  Plots a JAGS analysis model using the coda::plot.mcmc
  function.
}
\examples{
mod <- model("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")
dat <- data.frame(x = rpois(100,1))
ana <- analysis (mod, dat)
plot(ana)
}
\seealso{
  \code{\link{analysis}}, \code{\link[coda]{plot.mcmc}}
}

