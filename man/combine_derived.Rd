\name{combine_derived}
\alias{combine_derived}
\title{Combine derived iterations}
\usage{
  combine_derived(x, fun = sum, estimates = TRUE)
}
\arguments{
  \item{x}{a list of data frames with ...}

  \item{fun}{the function to use when combining the
  iterations - by default it sums the values for each
  iteration.}

  \item{estimates}{a logical scalar indicating whether to
  calculate the median and 95% credibility intervals or
  simply return the combined iterations.}
}
\value{
  a data frame with the median and 95% credibility
  intervals for the combined iterations (or the combined
  iterations themselves)
}
\description{
  Combines derived iterations
}

