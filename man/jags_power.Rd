\name{jags_power}
\alias{jags_power}
\title{Perform a JAGS simulation}
\usage{
  jags_power(model, data_model, values, nrep = 100,
    niter = 10^3, mode = "current")
}
\arguments{
  \item{data_model}{a \code{jags_data_model}.}

  \item{nrep}{an integer element indicating the number of
  datasets to generate for each set of input values.}

  \item{values}{a data.frame of input values.}

  \item{mode}{a character element indicating the mode for
  the analysis.}
}
\value{
  a \code{jags_simulation} object
}
\description{
  Performs a JAGS simulation by using a
  \code{jags_data_model} and a data frame of values to
  generate a simulation data frame using JAGS (Plummer
  2012).
}
\examples{
data_model <- jags_data_model("
data {
 for (i in 1:ny) {
   y[i] ~ dpois(bIntercept)
 }
}
")

model <- jags_data_model("
model {
bIntercept ~ dunif(0, 100)
 for (i in 1:ny) {
   y[i] ~ dpois(bIntercept)
 }
}
")

values <- data.frame(ny = c(10, 100), bIntercept = c(10,10))

# power <- jags_power (model, data_model, values = values, nrep = 100, mode = "demo")
}
\references{
  Plummer M (2012) JAGS Version 3.3.0 User Manual
  \url{http://sourceforge.net/projects/mcmc-jags/files/Manuals/}
}
\seealso{
  \code{\link{jags_data_model}} and
  \code{\link{jaggernaut}}
}

