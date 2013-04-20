\name{convergence}
\alias{convergence}
\title{Calculate convergence values}
\usage{
  convergence(object, model = 1, parameters = "fixed")
}
\arguments{
  \item{object}{a janalysis object}

  \item{model}{an integer element specifying the model to
  select. If model = 0 then it selects the model with the
  lowest DIC.}

  \item{parameters}{a character vector specifying the
  parameters for which to calculate the convergence}
}
\value{
  a data.frame of the parameters with their convergence
  (R-hat) values
}
\description{
  Calculates convergence (R-hat) values for the parameters
  in a JAGS analysis
}
\seealso{
  \code{\link{jaggernaut}} and \code{\link{analysis}}
}

