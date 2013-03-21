\name{convergence}
\alias{convergence}
\title{Calculate convergence for a JAGS analysis}
\usage{
  convergence(object, parameters = "fixed")
}
\arguments{
  \item{object}{a janalysis object}

  \item{parameters}{a character vector of the parameters to
  calculate the convergence}
}
\value{
  a data.frame of the parameter convergence
}
\description{
  Calculate convergence for a JAGS analysis
}
\examples{
model <- jmodel("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")
data <- data.frame(x = rpois(100,1))
analysis <- janalysis (model, data)
convergence(analysis)
convergence(analysis,c("bLambda"))
}

