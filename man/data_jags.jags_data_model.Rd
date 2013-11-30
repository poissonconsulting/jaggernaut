\name{data_jags.jags_data_model}
\alias{data_jags.jags_data_model}
\title{Get dataset from a JAGS data model}
\usage{
  \method{data_jags}{jags_data_model} (object, values, ...)
}
\arguments{
  \item{object}{a \code{jags_data_model} object.}

  \item{values}{a data.frame with a single row of data
  indicating the values for the simulation.}

  \item{...}{further arguments passed to or from other
  methods.}
}
\value{
  the simulated dataset in list form (unless modified by
  extract_data function).
}
\description{
  Simulates a dataset from a \code{jags_data_model} object.
}
\seealso{
  \code{\link{data_jags}} and \code{\link{jags_data_model}}
}

