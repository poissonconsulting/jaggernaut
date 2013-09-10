\name{add_jags}
\alias{add_jags}
\title{Add JAGS objects}
\usage{
  add_jags(object, object2, ...)
}
\arguments{
  \item{object}{a JAGS object.}

  \item{object2}{a second JAGS object to add to object.}

  \item{...}{additional JAGS objects to add to object.}
}
\value{
  a JAGS object of the original class
}
\description{
  Adds two or more JAGS object of the same class.
}
\seealso{
  \code{\link{add_jags.jags_model}},
  \code{\link{add_jags.jags_analysis}},
  \code{\link{add_jags.jags_simulation}} and
  \code{\link{add_jags.jags_power_analysis}}
}

