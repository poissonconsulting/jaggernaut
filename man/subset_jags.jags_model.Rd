\name{subset_jags.jags_model}
\alias{subset_jags.jags_model}
\title{Subset a JAGS model}
\usage{
  \method{subset_jags}{jags_model} (object, model_number,
    ...)
}
\arguments{
  \item{object}{a jags_model object.}

  \item{model_number}{an integer element indicating the
  model(s) to select.}

  \item{...}{other arguments passed to generic function.}
}
\value{
  the subsetted jags_analysis object
}
\description{
  Subset a JAGS model object.
}
\seealso{
  \code{\link{subset_jags}} and \code{\link{jags_analysis}}
}

