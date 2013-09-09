\name{data_jags.jags_data_model}
\alias{data_jags.jags_data_model}
\title{Get dataset from a JAGS data model}
\usage{
  \method{data_jags}{jags_data_model} (object, values, ...)
}
\arguments{
  \item{object}{a JAGS data model.}

  \item{values}{a data.frame with a single row of data
  indicating the values for the simulation.}

  \item{...}{other arguments passed to generic function.}
}
\value{
  the simulated dataset in list form.
}
\description{
  Simulates a dataset from a JAGS data model.
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

values <- data.frame(nx = 10, bIntercept = 5)

data_jags(data_model, values)
}
\seealso{
  \code{\link{data_jags}} and \code{\link{jags_data_model}}
}

