\name{data_jags.jags_simulation}
\alias{data_jags.jags_simulation}
\title{Get dataset(s) from a JAGS simulation}
\usage{
  \method{data_jags}{jags_simulation} (object, ...)
}
\arguments{
  \item{object}{a jags_simulation object.}

  \item{value}{an integer vector indicating the rows in
  values to select.}

  \item{rep}{an integer vector indicating the replicates to
  select.}

  \item{...}{other arguments passed to generic function.}
}
\value{
  a list or lists of the simulated data
}
\description{
  Gets the dataset(s) from a JAGS simulation object.
}
\examples{
data_model <- jags_data_model("
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

values <- data.frame(nx = c(1,10), bIntercept = c(5,10))

simulation <- jags_simulation (data_model, values, nrep = 5, mode = "test")

data_jags(simulation)
data_jags(simulation, value = 1, rep = NULL)
data_jags(simulation, value = NULL, rep = 1)
data_jags(simulation, value = NULL, rep = NULL)
}
\seealso{
  \code{\link{data_jags}} and \code{\link{jags_simulation}}
  #'
}

