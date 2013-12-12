\name{subset.jags_analysis}
\alias{subset.jags_analysis}
\title{Subset a JAGS analysis}
\usage{
  \method{subset}{jags_analysis} (object,
    model_number = NULL, ...)
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
  \code{\link{subset}} and \code{\link{jags_analysis}}
}

