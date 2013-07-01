\name{combine_jags_samples}
\alias{combine_jags_samples}
\title{Combine JAGS samples}
\usage{
  combine_jags_samples(object, by = NULL, fun = sum)
}
\arguments{
  \item{object}{a list of jags_sample objects.}

  \item{by}{the variables to combine by (using merge).}

  \item{fun}{the function to using when combining samples
  (by default fun = sum).}
}
\value{
  a jags_sample object
}
\description{
  Combines JAGS samples in multiple jags_sample objects by
  by using function fun
}
\seealso{
  \code{\link{predict.jags_analysis}}
}

