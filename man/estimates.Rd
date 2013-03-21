\name{estimates}
\alias{estimates}
\title{Calculate parameter estimates for a JAGS analysis}
\usage{
  estimates(object, parameters = "fixed", model = "min")
}
\arguments{
  \item{object}{a janalysis object}

  \item{parameters}{a character vector of the parameters to
  calculate the estimates}

  \item{model}{a character vector of length one indicating
  over which models to calculate the estimates.}
}
\value{
  a data.frame of the parameter estimates with the median
  estimate and lower and upper 95% credibility limits as
  well as the percent relative error and significance
}
\description{
  Calculate parameter estimates for a JAGS analysis
}
\examples{
model <- jmodel("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")
data <- data.frame(x = rpois(100,1))
analysis <- janalysis (model, data)
estimates(analysis)
}

