\name{calc_convergence.janalysis}
\alias{calc_convergence.janalysis}
\title{Calculate convergence values for JAGS analysis}
\usage{
  \method{calc_convergence}{janalysis} (object,
    summarise = TRUE, type = "all")
}
\arguments{
  \item{object}{a janalysis object}

  \item{summarise}{a boolean values indicating whether to
  provide a summary of the convergence or the convergence
  of each parameter value}

  \item{type}{a character value indicating whihc parameters
  to calculate the the convergence values for}
}
\value{
  a data.frame of the convergence values
}
\description{
  Calculates convergence values for JAGS analysis
}
\examples{
model <- jmodel("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")
data <- data.frame(x = rpois(100,1))
analysis <- janalysis (model, data)
calc_convergence(analysis, summarise = FALSE)
}

