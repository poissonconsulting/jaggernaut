\name{combine.jags_simulation}
\alias{combine.jags_simulation}
\title{Add JAGS simulation objects}
\usage{
  \method{combine}{jags_simulation} (object, ...,
    mode = "current")
}
\arguments{
  \item{object}{a \code{jags_simulation} object.}

  \item{...}{additional \code{jags_simulation} objects to
  add to \code{object}.}

  \item{mode}{a character element specifying the mode to
  use if generating replicates. See \code{opts_jagr} for
  further information}
}
\value{
  a \code{jags_simulation} object
}
\description{
  Adds two or more \code{jags_simulation} objects. If the
  objects have different numbers of replicates then extra
  replicates are generated until all the objects have the
  same number.
}
\seealso{
  \code{\link{combine}}, \code{\link{jags_simulation}},
  \code{\link{opts_jagr}} and \code{\link{jaggernaut}}.
}

