\name{base_values}
\alias{base_values}
\title{Get base values for JAGS analysis}
\usage{
  base_values(object)
}
\arguments{
  \item{object}{a janalysis object}
}
\value{
  a data.frame of the base values for the data used in the
  analysis
}
\description{
  Gets base values for JAGS analysis
}
\examples{
model <- jmodel("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")
data <- data.frame(x = rpois(100,1))
analysis <- janalysis (model, data)
}

