\name{jags_simulation}
\alias{jags_simulation}
\title{Create a JAGS simulation object}
\usage{
  jags_simulation(data_model, values, nrep = 100,
    mode = "current")
}
\arguments{
  \item{data_model}{a \code{jags_data_model}.}

  \item{values}{a data.frame of input values.}

  \item{nrep}{an integer element indicating the number of
  datasets to generate for each set of input values.}

  \item{mode}{a character element indicating the mode for
  the analysis.}
}
\value{
  a \code{jags_simulation} object
}
\description{
  Create a JAGS simulation object by generating data based
  on JAGS data model object and a set of values
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

simulation <- jags_simulation (data_model, values, nrep = 5)
number_of_replicates(simulation)
number_of_values(simulation)

simulation1 <- subset_jags(simulation, value = 1, rep = 1:2)
simulation1 <- update_jags(simulation1, nrep = 2)

simulation2 <- subset_jags(simulation, value = 2, rep = 1)

simulation <- add_jags(simulation1, simulation2)
number_of_replicates(simulation)
number_of_values(simulation)

data_jags(simulation)
data_jags(simulation, value = 1, rep = NULL)
data_jags(simulation, value = NULL, rep = 1)
data_jags(simulation, value = NULL, rep = NULL)
}
\seealso{
  \code{\link{jags_data_model}} and
  \code{\link{jaggernaut}}
}

