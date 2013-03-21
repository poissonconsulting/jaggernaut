\name{calc_estimates.janalysis}
\alias{calc_estimates.janalysis}
\title{Calculate parameter estimates for JAGS analysis}
\usage{
  calc_estimates.janalysis(object, parameters = "fixed")
}
\arguments{
  \item{object}{a janalysis object}

  \item{parameterss}{a character vector of the parameters
  to calculate the estimates}
}
\value{
  a data.frame of the parameter estimates with lower and
  upper 95% credibility limits as well as the percent
  relative error and significance
}
\description{
  Calculate parameter estimates for JAGS analysis
}
\examples{
model <- jmodel("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")
data <- data.frame(x = rpois(100,1))
analysis <- janalysis (model, data)
calc_estimates(analysis)
}

