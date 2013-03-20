\name{janalysis}
\alias{janalysis}
\title{Perform a janalysis (JAGS analysis)}
\usage{
  janalysis(models, data, n.iter = 1000, n.chain = 3,
    resample = 3, convergence = 1.1,
    parallelChains = .Platform$OS.type != "windows",
    parallelModels = .Platform$OS.type != "windows",
    debug = FALSE, quiet = FALSE)
}
\arguments{
  \item{models}{a jmodel object or list of jmodel objects
  specifying the JAGS model}

  \item{data}{a data.frame specifying the data to analyse}

  \item{n.iter}{the number of iterations in each mcmc
  chain}

  \item{n.chain}{the number of mcmc chains}

  \item{resample}{the number of times to resample until
  convergence is are achieved}

  \item{convergence}{the threshold for convergence}

  \item{parallelChains}{a boolean indicating whether the
  chains should be run on separate processes (currently
  only available for unix-based systems)}

  \item{parallelModels}{a boolean indicating whether the
  models should be run on separate processes (currently
  only available for unix-based systems)}

  \item{debug}{a boolean indicating whether or not to debug
  the model}

  \item{quiet}{a boolean indicating whether or not to
  suppress messages}
}
\value{
  a janalysis (JAGS analysis) object
}
\description{
  Performs a janalysis (JAGS analysis) by applying one or
  more jmodels (JAGS models) to a data frame. The remaining
  parameters control the number of iterations, the number
  of chains, the required convergence, whether the models
  and/or chains are run in parallel and the output
  messages. The janalysis object can then be queried using
  other functions to get parameter estimates, derived
  values and Deviance Information Criterion (DIC) values.
}
\details{
  The janalysis function performs a Bayesian analysis of a
  data frame using one or more jmodels (JAGS models). For
  ease of use only the jmodel(s) and data frame need to be
  defined. However, the number of iterations and the number
  of chains can be specified as can the number of times the
  model should be resampled until convergence is achieved.

  1000 chains drawn from the second halfs of the chains

  Convergence is achieved when all the monitored parmeters
  have an R-hat less than the value of the convergence
  argument which by default is 1.1. If the initial number
  of iterations are performed without achieving convergence
  and resample > 0 then just convergence doubled and a 1000
  iterations.

  If debug = TRUE then only two chains of 200 iterations
  are run on a single process and messages are not
  suppressed. This is to facilitate debugging of the JAGS
  model definition, i.e., the analysis part is as quick as
  possible and all messages are provided.
}
\examples{
model <- jmodel("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")
data <- data.frame(x = rpois(100,1))
analysis <- janalysis (model, data)
}

