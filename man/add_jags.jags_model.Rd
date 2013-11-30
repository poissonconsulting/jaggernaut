\name{add_jags.jags_model}
\alias{add_jags.jags_model}
\title{Add JAGS model objects}
\usage{
  \method{add_jags}{jags_model} (object, ...)
}
\arguments{
  \item{object}{a \code{jags_model} object.}

  \item{...}{additional \code{jags_model} objects to add to
  \code{object}.}
}
\value{
  a \code{jags_model} object with multiple models
}
\description{
  Adds two or more \code{jags_model} objects.
}
\seealso{
  \code{\link{add_jags}}, \code{\link{jags_model}} and
  \code{\link{jaggernaut}}.
}

