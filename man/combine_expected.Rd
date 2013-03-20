\name{combine_expected}
\alias{combine_expected}
\title{Combine expected iterations}
\usage{
  combine_expected(x, fun = sum, calc_estimates = TRUE)
}
\arguments{
  \item{x}{a list of data frames with ...}

  \item{fun}{the function to use when combining the
  iterations - by default it sums the values for each
  iteration.}

  \item{calc_estimates}{a logical scalar indicating whether
  or not to calculate the median and 95% credibility
  intervals or simply return the combined iterations.}
}
\value{
  a data frame with the median and 95% credibility
  intervals for the combined iterations (or the combined
  iterations themselves)
}
\description{
  Creates a JAGS model (jmodel) object which defines a
  Bayesian model in the JAGS dialect of the BUGS language.
  In addition to defining the model a JAGS model object can
  also specify the parameters to monitor, the variables to
  select, a function to manipulate the input data, a
  function to generate all (or some) of the initial values,
  a named list of the random parameters and the variables
  they are random with respect to and some code definition
  in the JAGS dialect to extract derived values from the
  final model among other things.
}
\details{
  Need more
}

