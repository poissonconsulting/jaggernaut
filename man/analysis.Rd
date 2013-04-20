\name{analysis}
\alias{analysis}
\alias{jags_analysis}
\title{Perform a JAGS analysis}
\usage{
  analysis(models, data, n_iters = 10^3, debug = FALSE)
}
\arguments{
  \item{models}{a \code{jags_model} or \code{jags_model}
  object specifying the JAGS model(s).}

  \item{data}{the data.frame to analyse.}

  \item{n_iters}{an integer element of the number of
  iterations to run per MCMC chain.}

  \item{debug}{a boolean element indicating whether or not
  to debug the model.}
}
\value{
  a \code{jags_analysis} object
}
\description{
  Performs a JAGS analysis by fitting a \code{jags_model}
  or \code{jags_models} object to a data frame using JAGS
  (Plummer 2012). The resultant \code{jags_analysis} object
  can then be passed to other function to to get the
  \code{convergence} of particular parameters, parameter
  \code{estimates} and \code{derived} parameter estimates.
}
\details{
  The \code{analysis} function performs a Bayesian analysis
  of a data frame for a \code{jags_model} or
  \code{jags_models} object. If \code{debug = FALSE} (the
  default) then the analysis options are as currently
  defined by \code{options_jaggernaut()} and each MCMC
  chain is run for \code{n.iter} iterations.  However if
  \code{debug = TRUE} then only two chains of 200
  iterations are run on a single process and messages are
  not suppressed. This is to facilitate debugging of the
  analysis.
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
}
\references{
  Plummer M (2012) JAGS Version 3.3.0 User Manual
  \url{http://sourceforge.net/projects/mcmc-jags/files/Manuals/}
}
\seealso{
  \code{\link{model}}, \code{\link{opts_jagr0}},
  \code{\link{convergence}}, \code{\link{estimates}},
  \code{\link{derived}} and \code{\link{jaggernaut}}
}

