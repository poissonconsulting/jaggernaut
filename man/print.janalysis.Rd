\name{print.janalysis}
\alias{print.janalysis}
\title{Print a JAGS analysis}
\usage{
  \method{print}{janalysis} (x, ...)
}
\arguments{
  \item{x}{a janalysis object to print}

  \item{...}{further arguments pass to or from other
  methods.}
}
\description{
  Prints a JAGS analysis
}
\examples{
mod <- model("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")
dat <- data.frame(x = rpois(100,1))
ana <- analysis (mod, dat)
print(ana)
}

