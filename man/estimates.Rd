\name{estimates}
\alias{estimates}
\title{Calculate parameter estimates}
\usage{
  estimates(object, model = 1, parameters = "fixed")
}
\arguments{
  \item{object}{a \code{janalysis} object}

  \item{model}{an integer element specifying the model to
  select. If model = 0 then it selects the model with the
  lowest DIC.}

  \item{parameters}{a character vector of the parameters to
  calculate the estimates}
}
\value{
  a data.frame of the parameter estimates with the median
  estimate and lower and upper 95\% credible limits as well
  as the percent relative error and significance
}
\description{
  Calculates parameter estimates for a JAGS analysis
}
\seealso{
  \code{\link{jaggernaut}}, \code{\link{analysis}},
  \code{\link{hm}}
}

