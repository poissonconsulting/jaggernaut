\name{data_jags.jags_power_analysis}
\alias{data_jags.jags_power_analysis}
\title{Get dataset(s) from a JAGS power}
\usage{
  \method{data_jags}{jags_power_analysis} (object,
    value = 1, rep = 1, ...)
}
\arguments{
  \item{object}{a jags_simulation object.}

  \item{based}{a logical element indicating xx.}

  \item{...}{other arguments passed to generic function.}
}
\value{
  a data.frame or list of the data used in the analysis
}
\description{
  Gets the dataset(s) from a JAGS power object.
}
\seealso{
  \code{\link{data_jags}} and \code{\link{jags_simulation}}
}

