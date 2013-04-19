\name{analysis}
\alias{analysis}
\alias{janalysis}
\title{Perform a JAGS analysis}
\usage{
  analysis(models, data, n.iter = 10^3, resample = 3,
    convergence = 1.1, debug = FALSE, quiet = FALSE,
    parallelChains = .Platform$OS.type != "windows",
    parallelModels = FALSE)
}
\arguments{
  \item{models}{a \code{jmodel} object or list of
  \code{jmodel} objects specifying the JAGS model.}

  \item{data}{a data.frame specifying the data to analyse.}

  \item{n.iter}{an integer element of the number of
  iterations per MCMC chain.}

  \item{resample}{an integer element of the number of times
  to resample until convergence is achieved.}

  \item{convergence}{a numeric element of the R-hat
  threshold for convergence.}

  \item{debug}{a boolean element indicating whether or not
  to debug the model}

  \item{quiet}{a boolean element indicating whether or not
  to suppress messages}

  \item{parallelChains}{a boolean element indicating
  whether the chains should be run on separate processes
  (currently only available for unix-based systems)}

  \item{parallelModels}{a boolean element indicating
  whether the models should be run on separate processes
  (currently only available for unix-based systems)}
}
\value{
  a \code{janalysis} (JAGS analysis) object
}
\description{
  Performs a JAGS analysis (\code{janalysis} object) by
  fitting a \code{jmodel} object or list of \code{jmodel}
  objects to a data frame using JAGS (Plummer 2012). The
  remaining parameters control the number of iterations,
  the required convergence, whether the models and/or
  chains are run in parallel and the output messages. The
  resultant \code{janalysis} object can then be queried
  using other functions to get the
  \code{\link{convergence}} of particular parameters,
  parameter \code{\link{estimates}} and
  \code{\link{derived}} parameter estimates.
}
\details{
  The \code{analysis} function performs a Bayesian analysis
  of a data frame with for a \code{jmodel} object or list
  of \code{jmodel} objects using JAGS. For ease of use only
  the model and data frame need to be defined. However, the
  number of iterations can be specified as can the number
  of times the model should be resampled until convergence
  is considered to have been achieved.

  The JAGS analysis retains 1,000 MCMC samples from the
  second halves of three MCMC chains. For example if and
  \code{n.iter = 1000}) then 333 samples will be thinned
  from the last 500 iterations of each chain.

  Convergence is considered to have been achieved when all
  the monitored parameters have an R-hat less than the
  value of the \code{convergence} argument which by default
  is 1.1 (Kery & Schaub 2011). If the initial number of
  iterations (\code{n.iter}) are performed and the
  convergence target has not been achieved and
  \code{resample} is greater than 0 then the value of
  \code{n.iter} is doubled, the MCMC sampling to date is
  considered the burn in period, the saved MCMC samples are
  discarded and MCMC sampling continues.  This process is
  continued until the convergence target is achieved or
  resampling exceed the value of the \code{resample}
  argument.

  If \code{debug = TRUE} then only two chains of 200
  iterations are run on a single process and messages are
  not suppressed. This is to facilitate debugging of the
  JAGS model definition, i.e., the analysis part is as
  quick as possible and all messages are provided.
}
\examples{
mod <- model("
model {
 bLambda ~ dlnorm(0,10^-2)
 for (i in 1:nrow) {
   x[i]~dpois(bLambda)
 }
}")

dat <- data.frame(x = rpois(100,1))

an <- analysis (mod, dat)

plot(an)
convergence(an)
estimates(an)
summary(an)
}
\references{
  Kery M & Schaub M (2011) Bayesian Population Analysis
  using WinBUGS. Academic Press.
  (\url{http://www.vogelwarte.ch/bpa})

  Plummer M (2012) JAGS Version 3.3.0 User Manual
  \url{http://sourceforge.net/projects/mcmc-jags/files/Manuals/}
}
\seealso{
  \code{\link{jaggernaut}}, \code{\link{model}},
  \code{\link{convergence}}, \code{\link{estimates}},
  \code{\link{derived}}
}

