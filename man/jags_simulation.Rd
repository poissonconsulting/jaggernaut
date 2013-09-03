\name{jags_simulation}
\alias{jags_simulation}
\title{Perform a JAGS simulation}
\usage{
  jags_simulation(model, values,
    quiet = opts_jagr("quiet"))
}
\arguments{
  \item{model}{a \code{jags_model}.}

  \item{values}{a data.frame of values.}

  \item{quiet}{a logical element indicating whether to
  print updates.}
}
\value{
  a \code{jags_simulation} object
}
\description{
  Performs a JAGS simulation by using a \code{jags_model}
  and a data frame of values to generate a simulation data
  frame using JAGS (Plummer 2012).
}
\examples{
mod <- jags_model("
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

val <- data.frame(nx = 10, bIntercept = 10)

sim <- jags_simulation (mod, val, quiet = TRUE)
}
\references{
  Plummer M (2012) JAGS Version 3.3.0 User Manual
  \url{http://sourceforge.net/projects/mcmc-jags/files/Manuals/}
}
\seealso{
  \code{\link{jags_model}} and \code{\link{jaggernaut}}
}

