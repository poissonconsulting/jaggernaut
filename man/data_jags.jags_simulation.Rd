\name{data_jags.jags_simulation}
\alias{data_jags.jags_simulation}
\title{Get dataset(s) from a JAGS simulation}
\usage{
  \method{data_jags}{jags_simulation} (object, value = 1,
    rep = 1, ...)
}
\arguments{
  \item{object}{a jags_simulation object.}

  \item{value}{an integer vector indicating the rows in
  values to select.}

  \item{rep}{an integer vector indicating the replicates to
  select.}

  \item{...}{other arguments passed to generic function.}
}
\value{
  a list or lists of the simulated data
}
\description{
  Gets the dataset(s) from a JAGS simulation object.
}
\seealso{
  \code{\link{data_jags}} and \code{\link{jags_simulation}}
}

