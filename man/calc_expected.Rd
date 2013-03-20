\name{calc_expected}
\alias{calc_expected}
\title{Calculate expected values for a JAGS analysis (janalysis object)}
\usage{
  calc_expected(analysis, parameter, data = "",
    base = FALSE, values = NULL, derived = NULL,
    random = NULL, length.out = 30, calc_estimates = T)
}
\arguments{
  \item{analysis}{a JAGS analysis (janalysis) object.}

  \item{parameter}{a character element naming the derived
  parameter of interest.}

  \item{data}{a data.frame of the data values over which to
  calculate the expected values of the derived parameter.
  If data.frame is "" xx.}

  \item{base}{a boolean element indicating whether or not
  to express the expected value as a percent change of a
  base level or a data frame defining the base level.}

  \item{values}{a data frame with a single row that defines
  the value of particular variables. The variables in data
  and base are replaced by the corresponding values.}

  \item{derived}{a character element defining a block in
  the JAGS dialect of the BUGS language that defines one or
  more derived parameter. If NULL the value is taken from
  the JAGS model for which the JAGS analysis was
  performed.}

  \item{random}{a named list which specifies which
  parameters to treat as random variables. If NULL the
  value is taken from the JAGS model for which the JAGS
  analysis was performed.}

  \item{length.out}{an integer element indicating the
  number of values when creating a sequence of values
  across the range of a continuous variable.}

  \item{calc_estimates}{a logical scalar indicating whether
  to return the individual iterations or the median and 95%
  credibility intervals.}
}
\value{
  the input data frame with the median and 95% credibility
  intervals (or iterations) for the derived parameter of
  interest
}
\description{
  Calculates expected values with 95% credibility intervals
  for a derived parameter.
}
\details{
  More information
}
\examples{
model <- jmodel(
 model = "model {
   bLambda ~ dunif(0,10)
   for (i in 1:nrow) {
     x[i]~dpois(bLambda)
   }
 }",
 derived = "model {
   for (i in 1:nrow) {
     eResidual[i] <- x[i] - bLambda
   }
 }",
 select = c("x")
)
data <- data.frame(x = rpois(100,1))
analysis <- janalysis (model, data)
calc_expected(analysis, "eResidual", data = NULL)
}

