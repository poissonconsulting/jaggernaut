\name{options_jaggernaut}
\alias{options_jaggernaut}
\alias{opts_jagr0}
\title{Get and set jaggernaut options}
\usage{
  options_jaggernaut(...)

  opts_jagr0(...)
}
\arguments{
  \item{...}{options can be defined using \code{name =
  value} or by passing a list of such tagged values.}
}
\value{
  For \code{opts_jagr0()} a list of all jaggernaut options
  values sorted by name. For \code{opts_jagr0(name)} a list
  of length one of the option value. When setting one or
  more options a list with the previous values of the
  options unchanged (returned invisibly).
}
\description{
  Queries and sets options for JAGS analyses.
}
\details{
  The function \code{options_jaggernaut()}, which can aso
  be invoked using the alias \code{opts_jagr0()}, behaves
  just like the \code{options()} function in the base
  library, with the additional feature that
  \code{opts_jagr0(default=TRUE)} will reset all options to
  the default values.

  Available options are

  \describe{ \item{convergence}{a numeric element of the
  R-hat threshold for convergence (default = 1.1)}
  \item{n_chains}{a integer element indciating the number
  of MCMC chains (default = 3)} \item{n_resamples}{an
  integer element of the number of times to resample until
  convergence is achieved (default = 3)} #'
  \item{parallel_chains}{a boolean element indicating
  whether the chains should be run on separate processes
  (default is platform dependennt)}
  \item{parallel_models}{a boolean element indicating
  whether the models should be run on separate processes
  (default = FALSE)} \item{quiet}{a boolean element
  indicating whether or not to suppress messages (default =
  FALSE)} }

  By default a JAGS analysis will retain a minumum of 1,000
  MCMC samples thinned from the second halves of three
  chains. For example if \code{n_iters = 1000} in the
  analysis then by default 334 samples will be thinned from
  the last 500 iterations of each chain.

  Convergence is considered to have been achieved when all
  the monitored parameters have an R-hat less than the
  value of the \code{convergence} option which by default
  is 1.1 (Kery & Schaub 2011). If the initial number of
  iterations (\code{n_iters}) are performed and the
  convergence target has not been achieved and the
  \code{n_resamples} option is greater than 0 then the
  value of \code{n_iters} is doubled, the MCMC sampling to
  date is considered the burn in period, the saved MCMC
  samples are discarded and MCMC sampling continues.  This
  process is continued until the convergence target is
  achieved or resampling exceeds the value of the
  \code{n_resamples} option.

  Currently parallel processing is only available for
  unix-based systems. For such systems the
  \code{parallel_chains} option is by default \code{TRUE}
  otherwise its \code{FALSE}.
}
\examples{
opts_jagr0()
opts_jagr0(n_chains = 4)
opts_jagr0("n_chains")
opts_jagr0(default = TRUE)
options_jaggernaut("n_chains")
}
\seealso{
  \code{\link{analysis}} and \code{\link{options}}
}

