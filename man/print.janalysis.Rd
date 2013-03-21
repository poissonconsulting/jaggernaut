\name{print.janalysis}
\alias{print.janalysis}
\title{Print a JAGS analysis}
\usage{
  print.janalysis(x, model = "min")
}
\arguments{
  \item{x}{a janalysis object to print}
}
\description{
  Prints a JAGS analysis
}
\examples{
model <- jmodel("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")
data <- data.frame(x = rpois(100,1))
analysis <- janalysis (model, data)
print(analysis)
}

