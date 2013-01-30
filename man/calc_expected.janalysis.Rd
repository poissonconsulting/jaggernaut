\name{calc_expected.janalysis}
\alias{calc_expected.janalysis}
\title{Calculate expected values for JAGS analysis}
\usage{
  \method{calc_expected}{janalysis} (analysis, parameter,
    data = "", base = FALSE, derived = NULL, random = NULL,
    length.out = 30)
}
\arguments{
  \item{analysis}{a JAGS analysis object.}

  \item{parameter}{a character element naming the derived
  parameter of interest.}

  \item{data}{a data.frame of the data values over which to
  calculate the expected values of the derived parameter.
  If data.frame is "" xx.}

  \item{base}{a boolean element indicating whether or not
  to express the expected value as a percent change.}

  \item{derived}{a character element defining a block in
  the JAGS dialect of the BUGS language that defines one or
  more derived parameter. If NULL the value is taken from
  the JAGS model for which the JAGS analysis was
  performed.}

  \item{random}{a named list which specifies which
  parameters to treat as random variables.}

  \item{length.out}{an integer element indicating the
  number of values when creating a sequence of values
  across the range of a continuous variable.}
}
\description{
  Calculates expected values with 95% credibility intervals
  for a derived parameter.
}
\examples{
model <- jmodel(
 model = "model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }",
 derived = "model { for (i in 1:nrow) {
 eResidual[i] <- x[i] - bLambda
 } }"
)
data <- data.frame(x = rpois(100,1))
analysis <- janalysis (model, data)
calc_expected(analysis, "eResidual", data = NULL)
}

