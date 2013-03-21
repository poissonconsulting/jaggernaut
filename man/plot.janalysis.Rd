\name{plot.janalysis}
\alias{plot.janalysis}
\title{Plot a JAGS analysis}
\usage{
  plot.janalysis(x, parameters = "all", ...)
}
\arguments{
  \item{x}{a janalysis object}

  \item{parameters}{a character vector of the parameters to
  plot or "all", "fixed" or "random". Not yet implemented.}

  \item{...}{arguments to pass to coda::plot.mcmc}
}
\description{
  Plots the JAGS analysis model with the lowest DIC using
  the coda::plot.mcmc function.
}
\examples{
model <- jmodel("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")
data <- data.frame(x = rpois(100,1))
analysis <- janalysis (model, data)
plot(analysis)
}
\seealso{
  \link[jaggernaut]{jmodel}, \link[jaggernaut]{janalysis},
  \link[coda]{plot.mcmc}
}

