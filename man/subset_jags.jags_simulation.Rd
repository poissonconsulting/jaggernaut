\name{subset_jags.jags_simulation}
\alias{subset_jags.jags_simulation}
\title{Subset a JAGS simulation}
\usage{
  \method{subset_jags}{jags_simulation} (object,
    value = NULL, rep = NULL, ...)
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
  the subsetted JAGS simulation object.
}
\description{
  Subset a JAGS simulation object.
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

subset_jags(simulation)
subset_jags(simulation, value = 1, rep = NULL)
subset_jags(simulation, value = NULL, rep = 1)
subset_jags(simulation, value = 1, rep = 1)
}
\seealso{
  \code{\link{subset_jags}} and
  \code{\link{jags_simulation}} #'
}

