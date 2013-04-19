\name{dataset}
\alias{dataset}
\title{Get data from a JAGS analysis}
\usage{
  dataset(object, base = FALSE)
}
\arguments{
  \item{object}{a janalysis object}

  \item{base}{a logical scalar indicating whether to return
  the base values}
}
\value{
  a data frame of the data used in the analysis or the
  corresponding base values
}
\description{
  Returns the input data set from a JAGS analysis or
  generates a data set with one row specify the base values
  for each of the variables. For further information on the
  use of base values see the \code{derived} function.
}
\seealso{
  \code{\link{jaggernaut}}, \link{analysis}, \link{derived}
}

