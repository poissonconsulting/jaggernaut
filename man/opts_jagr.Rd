\name{opts_jagr}
\alias{options_jaggernaut}
\alias{opts_jagr}
\title{Get and set jaggernaut options}
\usage{
  opts_jagr(...)

  options_jaggernaut(...)
}
\arguments{
  \item{...}{options can be defined using \code{name =
  value} or by passing a list of such tagged values.}
}
\value{
  For \code{opts_jagr()} a list of all jaggernaut options
  values sorted by name. For \code{opts_jagr(name)} a list
  of length one of the option value. When setting one or
  more options a list with the previous values of the
  options unchanged (returned invisibly).
}
\description{
  Queries and sets options for JAGS analyses.
}
\details{
  The function \code{opts_jagr()}, which can aso be invoked
  using its alias \code{options_jaggernaut()}, behaves just
  like the \code{options()} function in the base library,
  with the additional feature that
  \code{opts_jagr(mode="default")} will reset all options
  to the values for the default mode. There are four
  available modes: debug, explore, report and paper which
  are characterized by increasing accuracy. In summary the
  debug model should be used when first trying to code
  models in the JAGS dialect of the BUGS language. Once the
  JAGS code is running without errors is now time to switch
  to the explore model to look at model adequacy. Once you
  are content that the model is adequate you can now switch
  to report model to extract the results for presentation
  in a report or paper model if you are going to be sending
  the results to a peer-reviewed journal. The default mode
  is report mode.

  Available options are \describe{ \item{level}{the
  credible interval level (default = 0.95)}
  \item{nchains}{the number of MCMC chains (default = 3)}
  \item{nresample}{the number of times to resample until
  convergence is achieved (default = 3)} \item{nsims}{the
  total number of MCMC samples to thin from the second
  halves of the MCMC chains (default = 1000)}
  \item{parallel_chains}{whether the chains should be run
  on separate processes (default is platform dependennt)}
  \item{parallel_models}{whether the models should be run
  on separate processes (default = FALSE)}
  \item{quiet}{whether to suppress messages (default =
  FALSE)} \item{rhat}{the R-hat threshold for convergence
  (default = 1.1)} }

  By default a JAGS analysis will retain a minumum of 1,000
  MCMC samples thinned from the second halves of three
  chains. For example if \code{niter = 1000} in the
  analysis then by default 334 samples will be thinned from
  the last 500 iterations of each chain.

  Convergence is considered to have been achieved when all
  the monitored parameters have an R-hat less than the
  value of the \code{rhat} option which by default is 1.1
  (Kery & Schaub 2011). If the initial number of iterations
  are performed and the convergence target has not been
  achieved and \code{nresample > 0} then the value of
  \code{niter} is doubled, the MCMC sampling to date is
  considered the burn in period, the saved MCMC samples are
  discarded and MCMC sampling continues.  This process is
  repeated until the convergence target is achieved or
  resampling would exceed the value of the \code{nresample}
  argument.

  Currently parallel processing is only available for
  unix-based systems. For such systems the
  \code{parallel_chains} option is by default \code{TRUE}
  otherwise its \code{FALSE}.
}
\examples{
opts_jagr()
opts_jagr(mode = "debug")
options_jaggernaut()
opts_jagr("nchains","mode")
opts_jagr(nchains = 4)
opts_jagr("nchains","mode")
old <- opts_jagr(mode = "default")
opts_jagr()
opts_jagr(old)
opts_jagr("nchains","mode")
}
\seealso{
  \code{\link{jags_analysis}} and \code{\link{options}}
}

