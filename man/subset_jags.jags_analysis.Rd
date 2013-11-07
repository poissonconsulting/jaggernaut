\name{subset_jags.jags_analysis}
\alias{subset_jags.jags_analysis}
\title{Subset a JAGS analysis}
\usage{
  \method{subset_jags}{jags_analysis} (object,
    model_number = 0, ...)
}
\arguments{
  \item{object}{a jags_analysis object.}

  \item{model_number}{an integer element indicating the
  model(s) to select.}

  \item{...}{other arguments passed to generic function.}
}
\value{
  the subsetted jags_analysis object
}
\description{
  Subset a JAGS analysis object.
}
\seealso{
  \code{\link{subset_jags}} and \code{\link{jags_analysis}}
}

