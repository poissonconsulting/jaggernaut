\name{ind.jags_analysis}
\alias{ind.jags_analysis}
\title{Get R-hat value(s)}
\usage{
  \method{ind}{jags_analysis} (object, parm = "all",
    combine = TRUE, ...)
}
\arguments{
  \item{object}{a JAGS analysis}

  \item{character}{element}

  \item{...}{passed to and from other functions}
}
\value{
  a vector, matrix or array of ind values

  model <- jags_model(" model { bLambda ~ dlnorm(0,10^-2)
  for (i in 1:nrow) { x[i]~dpois(bLambda) } }")

  data <- data.frame(x = rpois(100,1))

  analysis <- jags_analysis (model, data, mode = "demo")
  ind(analysis)
}
\description{
  Get R-hat value or values for JAGS analysis
}

