\name{print.jmodel}
\alias{print.jmodel}
\title{Print a JAGS model}
\usage{
  print.jmodel(x)
}
\arguments{
  \item{x}{a jmodel object to print}
}
\description{
  Prints a JAGS model
}
\examples{
model <- jmodel("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")
print(model)
}

