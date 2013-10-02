\name{rhat.jags_analysis}
\alias{rhat.jags_analysis}
\title{Get R-hat value(s)}
\usage{
  \method{rhat}{jags_analysis} (object, parm = "all",
    combine = TRUE, ...)
}
\arguments{
  \item{object}{a JAGS analysis}

  \item{parm}{a character vector indicating the parameters
  for which to calculate the rhat values}

  \item{combine}{a logical element indicating whether to
  return the rhat values for each parameter (combine =
  FALSE) or the maximum of the rhat values (combine = TRUE,
  i.e., the default)}

  \item{...}{passed to and from other functions}
}
\value{
  a vector, matrix or array of rhat values

  model <- jags_model(" model { bLambda ~ dlnorm(0,10^-2)
  for (i in 1:nrow) { x[i]~dpois(bLambda) } }")

  data <- data.frame(x = rpois(100,1))

  analysis <- jags_analysis (model, data, mode = "demo")
  rhat(analysis)
}
\description{
  Get R-hat value or values for JAGS analysis
}

