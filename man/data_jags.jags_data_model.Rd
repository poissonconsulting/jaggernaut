\name{data_jags.jags_data_model}
\alias{data_jags.jags_data_model}
\title{Get dataset from a JAGS data model}
\usage{
  \method{data_jags}{jags_data_model} (object, values, ...)
}
\arguments{
  \item{object}{a JAGS data model.}

  \item{values}{a data.frame with a single row of data
  indicating the values for the simulation.}

  \item{...}{other arguments passed to generic function.}
}
\value{
  the simulated dataset in list form (unless modified by
  extract_data function).
}
\description{
  Simulates a dataset from a JAGS data model.
}
\seealso{
  \code{\link{data_jags}} and \code{\link{jags_data_model}}
}

