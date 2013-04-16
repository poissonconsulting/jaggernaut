\name{janalysis}
\alias{janalysis}
\title{Create a JAGS analysis}
\usage{
  janalysis(models, data, n.iter = 1000, n.chain = 3,
    resample = 3, convergence = 1.1,
    parallelChains = .Platform$OS.type != "windows",
    parallelModels = .Platform$OS.type != "windows",
    debug = FALSE, quiet = FALSE)
}
\arguments{
  \item{models}{a \code{\link{jmodel}} object or list of
  \code{\link{jmodel}} objects specifying the JAGS model}

  \item{data}{a data.frame specifying the data to analyse}

  \item{n.iter}{an integer element of the number of
  iterations in each MCMC chain}

  \item{n.chain}{an integer element of the number of MCMC
  chains}

  \item{resample}{an integer element of the number of times
  to resample until convergence is achieved}

  \item{convergence}{a numeric element of the R-hat
  threshold for convergence}

  \item{parallelChains}{a boolean element indicating
  whether the chains should be run on separate processes
  (currently only available for unix-based systems)}

  \item{parallelModels}{a boolean element indicating
  whether the models should be run on separate processes
  (currently only available for unix-based systems)}

  \item{debug}{a boolean element indicating whether or not
  to debug the model}

  \item{quiet}{a boolean element indicating whether or not
  to suppress messages}
}
\value{
  a \code{janalysis} (JAGS analysis) object
}
\description{
  Creates a JAGS analysis (\code{janalysis} object) by
  fitting a \code{\link{jmodel}} or list of \code{jmodel}
  objects to a data frame using JAGS (Plummer 2012). The
  remaining parameters control the number of iterations,
  the number of chains, the required convergence, whether
  the models and/or chains are run in parallel and the
  output messages. The \code{janalysis} object can then be
  queried using other functions to get parameter
  \code{\link{estimates}} and \code{\link{derived}} values.
}
\details{
  The \code{janalysis} function performs a Bayesian
  analysis of a data frame with one or more \code{jmodels}
  using JAGS. For ease of use only the
  \code{\link{jmodel}}(s) and data frame need to be
  defined. However, the number of iterations and the number
  of chains can be specified as can the number of times the
  model should be resampled until convergence is achieved.

  1000 chains drawn from the second halfs of the chains

  Convergence is achieved when all the monitored parmeters
  have an R-hat less than the value of the convergence
  argument which by default is 1.1. If the initial number
  of iterations are performed without achieving convergence
  and resample > 0 then just convergence doubled and a 1000
  iterations.

  If \code{debug = TRUE} then only two chains of 200
  iterations are run on a single process and messages are
  not suppressed. This is to facilitate debugging of the
  JAGS model definition, i.e., the analysis part is as
  quick as possible and all messages are provided.
}
\examples{
model <- jmodel("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")
data <- data.frame(x = rpois(100,1))
analysis <- janalysis (model, data)
}
\references{
  Kery & Schaub (2011) Bayesian Population Analysis using
  WinBUGS. Academic Press, Oxford.
  \url{http://store.elsevier.com/Bayesian-Population-Analysis-using-WinBUGS/Marc-Kery/isbn-9780123870209/}

  Plummer M (2012) JAGS Version 3.3.0 User Manual
  \url{http://sourceforge.net/projects/mcmc-jags/files/Manuals/}
}
\seealso{
  \code{\link{jmodel}}, \code{\link{estimates}},
  \code{\link{derived}}
}

