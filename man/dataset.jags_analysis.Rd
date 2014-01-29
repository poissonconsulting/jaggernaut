\name{dataset.jags_analysis}
\alias{dataset.jags_analysis}
\title{Get dataset from a JAGS analysis object}
\usage{
\method{dataset}{jags_analysis}(object, converted = FALSE, ...)
}
\arguments{
  \item{object}{a \code{jags_analysis} object.}

  \item{...}{further arguments passed to or from other
  methods.}

  \item{converted}{a logical scalar indicating whether the
  data should be converted.}
}
\value{
The original dataset.
}
\description{
Returns the original dataset from a \code{jags_analysis}
object.
}
\seealso{
\code{\link{dataset}}, \code{\link{jags_analysis}} and
\code{\link{jaggernaut}}
}

