\name{rhat}
\alias{rhat}
\title{Get R-hat value(s)}
\usage{
rhat(object, parm, combine, ...)
}
\arguments{
  \item{object}{a JAGS object}

  \item{parm}{a character vector indicating the parameters
  for which to calculate the R-hat values}

  \item{combine}{a logical element indicating whether or
  not to summarise by the maximum R-hat value.}

  \item{...}{passed to and from other functions}
}
\value{
a vector, matrix or array of rhat values
}
\description{
Get R-hat value(s) for JAGS objects
}

