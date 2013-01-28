\name{calc_convergence.janalysis}
\alias{calc_convergence.janalysis}
\title{Calculate convergence values for JAGS analysis}
\usage{
  \method{calc_convergence}{janalysis} (object,
    summarise = TRUE, type = "all")
}
\arguments{
  \item{object}{a janalysis object}

  \item{summarise}{a boolean value indicating whether or
  not to summarise}

  \item{type}{xx}
}
\value{
  xx
}
\description{
  Calculate convergence values for JAGS analysis
}
\examples{
model <- jmodel("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")
data <- data.frame(x = rpois(100,1))
analysis <- janalysis (model, data)
calc_convergence(analysis)
}

