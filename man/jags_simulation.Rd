\name{jags_simulation}
\alias{jags_simulation}
\title{Perform a JAGS simulation}
\usage{
  jags_simulation(data_model, values, nrep = 1,
    mode = "current")
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
mod <- jags_data_model("
data {
 for (i in 1:nx) {
   x[i] ~ dpois(bIntercept)
   for (j in 1:nx) {
     y[i,j] ~ dpois(bIntercept)
   }
 }
 z <- bIntercept
}
")

val <- data.frame(nx = c(1,10), bIntercept = c(5,10))

sim <- jags_simulation (mod, val, nrep = 5)

sim1 <- subset_jags(sim, value = 1, rep = 1:2)
sim1 <- update_jags(sim1, nrep = 2)

sim2 <- subset_jags(sim, value = 2, rep = 1)

sims <- add_jags(sim1, sim2)

data_jags(sims)
}
\references{
  Plummer M (2012) JAGS Version 3.3.0 User Manual
  \url{http://sourceforge.net/projects/mcmc-jags/files/Manuals/}
}
\seealso{
  \code{\link{jags_data_model}} and
  \code{\link{jaggernaut}}
}

