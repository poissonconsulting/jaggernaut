\name{DIC}
\alias{DIC}
\title{Calculate Deviance Information Criterion values for JAGS analysis}
\usage{
  DIC(object)
}
\arguments{
  \item{object}{a janalysis object}
}
\value{
  a table of the Deviance Information Criterion values by
  model number
}
\description{
  Calculates Deviance Information Criterion values for JAGS
  analysis
}
\examples{
model <- jmodel("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")
data <- data.frame(x = rpois(100,1))
analysis <- janalysis (model, data)
DIC(analysis)
}

