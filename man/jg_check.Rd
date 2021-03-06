% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check.R
\name{jg_check}
\alias{jg_check}
\title{Check JAGS model code}
\usage{
jg_check(x, extended = FALSE)
}
\arguments{
\item{x}{string of JAGS model code}

\item{extended}{flag of whether to allow extended BUGS language}
}
\value{
Invisible flag of whether JAGS model code passes certain checks.
In addition, a unique warning is issued for each failed check.
}
\description{
Checks JAGS model code
}
