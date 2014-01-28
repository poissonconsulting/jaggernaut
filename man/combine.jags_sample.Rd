\name{combine.jags_sample}
\alias{combine.jags_sample}
\title{Combine JAGS samples}
\usage{
\method{combine}{jags_sample}(object, ..., by = NULL, fun = sum)
}
\arguments{
  \item{object}{a \code{jags_sample} object.}

  \item{...}{additional \code{jags_sample} objects to add
  to \code{object}.}

  \item{by}{the variables to combine by (using merge).}

  \item{fun}{the function to using when combining samples
  (by default fun = sum).}
}
\value{
a jags_sample object #'
}
\description{
Combine JAGS samples in multiple jags_sample objects by by
using function fun
}
\seealso{
\code{\link{combine}}, \code{\link{predict.jags_analysis}}
and \code{\link{ddply_jags_sample}}
}

