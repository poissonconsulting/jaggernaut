\name{subset.jags_simulation}
\alias{subset.jags_simulation}
\title{Subset a JAGS simulation}
\usage{
\method{subset}{jags_simulation}(x, value = NULL, rep = NULL, ...)
}
\arguments{
  \item{x}{a jags_simulation x.}

  \item{value}{an integer vector indicating the rows in
  values to select.}

  \item{rep}{an integer vector indicating the replicates to
  select.}

  \item{...}{other arguments passed to generic function.}
}
\value{
the subsetted JAGS simulation x.
}
\description{
Subset a JAGS simulation x.
}
\seealso{
\code{\link{subset}} and \code{\link{jags_simulation}}
}

