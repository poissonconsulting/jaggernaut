\name{combine_derived}
\alias{combine_derived}
\title{Combine derived iterations}
\usage{
  combine_derived(x, fun = sum, conf_int = TRUE)
}
\arguments{
  \item{x}{a list of data frames with ...}

  \item{fun}{the function to use when combining samples of
  a derived parameter - by default it sums the values for
  each sample.}

  \item{conf_int}{a logical scalar indicating whether to
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

