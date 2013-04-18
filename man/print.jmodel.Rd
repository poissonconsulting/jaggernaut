\name{print.jmodel}
\alias{print.jmodel}
\title{Print a JAGS model}
\usage{
  \method{print}{jmodel} (x, ...)
}
\arguments{
  \item{x}{a jmodel object to print}

  \item{...}{further arguments pass to or from other
  methods.}
}
\description{
  Prints a JAGS model
}
\examples{
mod <- model("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")
print(mod)
}

