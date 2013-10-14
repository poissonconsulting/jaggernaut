\name{jags_power_analysis}
\alias{jags_power_analysis}
\title{Perform a JAGS power analysis}
\usage{
  jags_power_analysis(model, data_model, values,
    nreps = 100, niters = 10^3, mode = "current")
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
data_model <- jags_data_model("data {
 for (gp in 1:nGroup) {
  bGroup[gp] ~ dnorm(0, sGroup^-2)
  for (i in 1:nSample) {
    z[gp, i] ~ dbern(bProp)
    y[gp, i] ~ dnorm(bGroup[gp] + bEffect * z[gp, i],sSample^-2)
  }
 }
} ",
 extract_data = function (data) {
 data$bGroup <- NULL
 data$sGroup <- NULL
 data$sSample <- NULL
 data$bEffect <- NULL
 data$bProp <- NULL
 return (data)
})

values <- expand.grid(nGroup = 10,
sGroup = 0.1,
nSample = 10,
sSample = 0.1,
bEffect = c(0,0.01),
bProp = 0.5)

model <- jags_model("model {

sGroup ~ dunif(0,1)
sSample ~ dunif(0,1)
bEffect ~ dnorm(0, 1^-2)
bProp ~ dunif(0, 1)
for (gp in 1:nGroup) {
 bGroup[gp] ~ dnorm(0, sGroup^-2)
 for (i in 1:nSample) {
   z[gp, i] ~ dbern(bProp)
   y[gp, i] ~ dnorm(bGroup[gp] + bEffect * z[gp, i],sSample^-2)
 }
}
}",
random_effects = list(bGroup = NULL))

power <- jags_power_analysis(model, data_model, values, nreps = 5, mode = "demo")

print(power)
nvalues(power)
nreps(power)
nchains(power)
niters(power)
nsims(power)
rhat(power)
is_converged(power, percent = TRUE)

values$bEffect <- c(0.025,0.05)

power <- update_jags(power, nreps = 10, values = values)
rhat(power)
opts <- opts_jagr()
opts_jagr(rhat = 1.05)
power <- update_jags(power, mode = "current")
opts_jagr(opts)
rhat(power)
power_jags(power, parm = list(bEffect = c("significance < 0.05")))

coef(power, combine = TRUE)
}
\references{
  Plummer M (2012) JAGS Version 3.3.0 User Manual
  \url{http://sourceforge.net/projects/mcmc-jags/files/Manuals/}
}
\seealso{
  \code{\link{jags_data_model}}, \code{\link{jags_model}}
  and \code{\link{jaggernaut}}
}

