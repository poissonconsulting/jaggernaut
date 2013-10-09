\name{jags_power_analysis}
\alias{jags_power_analysis}
\title{Perform a JAGS power analysis}
\usage{
  jags_power_analysis(model, data_model, values,
    nreps = 100, niters = 10^3, parm = c(fixed = 0),
    mode = "current")
}
\arguments{
  \item{model}{a \code{jags_model}.}

  \item{data_model}{a \code{jags_data_model}.}

  \item{values}{a data.frame of input values.}

  \item{nreps}{an integer element indicating the number of
  datasets to generate for each set of input values.}

  \item{niters}{an integer element indicating the number of
  iterations.}

  \item{mode}{a character element indicating the mode for
  the analysis.}
}
\value{
  a \code{jags_power_analysis} object
}
\description{
  Performs a JAGS power analysis.
}
\examples{
data_model <- jags_data_model("
data {
 for (i in 1:ny) {
   y[i] ~ dpois(bIntercept)
 }
}
")

model <- jags_model("
model {
bIntercept ~ dunif(0, 100)
 for (i in 1:length(y)) {
   y[i] ~ dpois(bIntercept)
 }
} ",
select = c("y"))

values <- data.frame(ny = c(10, 100), bIntercept = c(10,10))

power <- jags_power_analysis (model = model, data_model = data_model,
                             values = values, nreps = 10, mode = "demo")
nvalues(power)
nreps(power)
nchains(power)
niters(power)
nsims(power)
rhat(power)
is_converged(power, percent = TRUE)

power_jags(power)
}
\references{
  Plummer M (2012) JAGS Version 3.3.0 User Manual
  \url{http://sourceforge.net/projects/mcmc-jags/files/Manuals/}
}
\seealso{
  \code{\link{jags_data_model}}, \code{\link{jags_model}}
  and \code{\link{jaggernaut}}
}

