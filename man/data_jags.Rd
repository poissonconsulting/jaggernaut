\name{data_jags}
\alias{data_jags}
\title{Get dataset(s) from a JAGS object}
\usage{
  data_jags(object, ...)
}
\arguments{
  \item{object}{a JAGS object.}

  \item{...}{further arguments passed to or from other
  methods.}
}
\value{
  a data.frame, object of type \code{jags_data} or list(s)
  of the data
}
\description{
  Gets the dataset(s) from a JAGS object.
}
\seealso{
  \code{\link{data_jags.jags_data_model}}
}

