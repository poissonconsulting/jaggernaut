\name{jags_analysis}
\alias{jags_analysis}
\title{Perform a JAGS analysis}
\usage{
  jags_analysis(model, data, niter = 10^3,
    mode = "current")
}
\arguments{
  \item{model}{a \code{jags_model} specifying the JAGS
  model(s).}

  \item{data}{the data.frame or list of data to analyse.}

  \item{niter}{an integer element of the number of
  iterations to run per MCMC chain.}

  \item{mode}{a character element indicating the mode for
  the analysis.}
}
\value{
  a \code{jags_analysis} object
}
\description{
  Performs a JAGS analysis by fitting a \code{jags_model}
  or list of \code{jags_model}s to a data frame using JAGS
  (Plummer 2012). The resultant \code{jags_analysis} object
  can then be passed to other functions to to get the
  \code{rhat} of particular parameters, parameter
  \code{coefficients} with credible intervals and
  \code{predict} derived parameter estimates.
}
\details{
  The \code{jags_analysis} function performs a Bayesian
  analysis of a data frame for a \code{jags_model} or list
  of \code{jags_model}s.

  If \code{mode = "current"} (the default) then the
  analysis options are as currently globally defined by
  \code{opts_jagr()} otherwise the \code{mode} argument
  specifies the analysis mode for that particular analysis.

  The \code{niter} argument specifies the total number of
  iterations including adaptive and burn in periods for
  each chain. The only exceptions are when the analysis is
  in debug mode in which case \code{niter} is set to be 100
  or if \code{niter} is less than \code{nsims * 2 / nchain}
  (where nsims is set by the mode) in which case
  \code{niter} is set to be \code{nsims * 2 / nchain} so
  that \code{nsims} can be drawn from the second halves of
  the chains.
}
\examples{
model <- jags_model("
model {
 bLambda ~ dlnorm(0,10^-2)
 for (i in 1:nrow) {
   x[i]~dpois(bLambda)
 }
}")

data <- data.frame(x = rpois(100,1))

analysis <- jags_analysis (model, data, mode = "demo")

analysis <- update_jags(analysis, mode = "demo")
}
\references{
  Plummer M (2012) JAGS Version 3.3.0 User Manual
  \url{http://sourceforge.net/projects/mcmc-jags/files/Manuals/}
}
\seealso{
  \code{\link{jags_model}}, \code{\link{opts_jagr}},
  \code{\link{rhat}}, \code{\link{coef.jags_analysis}},
  \code{\link{predict.jags_analysis}} and
  \code{\link{jaggernaut}}
}

