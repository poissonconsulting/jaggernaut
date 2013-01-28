\name{janalysis}
\alias{janalysis}
\title{Perform JAGS analysis}
\usage{
  janalysis(model, data, n.iter = 1000, n.chain = 3,
    resample = 3, convergence = 1.1, independence = 0,
    parallel = FALSE, debug = FALSE, quiet = FALSE)
}
\arguments{
  \item{model}{a jmodel object specifying the JAGS model}

  \item{data}{a data.frame specifying the data to analyse}

  \item{n.iter}{the number of iterations in each mcmc
  chain}

  \item{n.chain}{the number of mcmc chains}

  \item{resample}{the number of times to resample until
  convergence and independence are achieved}

  \item{convergence}{the threshold for convergence}

  \item{independence}{the threshold for indepedence}

  \item{parallel}{a boolean indicating whether the analysis
  should be run on parallel processors}

  \item{debug}{a boolean indicating whether or not the
  intent is to debug the model}

  \item{quiet}{a boolean indicating whether or not to
  suppress messages}
}
\value{
  a JAGS analysis object
}
\description{
  Performs JAGS analysis
}
\examples{
model <- jmodel("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")
data <- data.frame(x = rpois(100,1))
analysis <- janalysis (model, data)
}

