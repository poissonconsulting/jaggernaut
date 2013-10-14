\name{jags_simulation}
\alias{jags_simulation}
\title{Create a JAGS simulation object}
\usage{
  jags_simulation(data_model, values, nreps = 100,
    mode = "current")
}
\arguments{
  \item{data_model}{a \code{jags_data_model}.}

  \item{values}{a data.frame of input values.}

  \item{nreps}{an integer element indicating the number of
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
model <- jags_data_model("
data {
 bLambda ~ dunif(3,6)
 for (i in 1:nx) {
  x[i]~dpois(bLambda)
 }
}")

values <- data.frame(nx = c(2,10))
simulation <- jags_simulation (model, values, nreps = 5, mode = "demo")

print(simulation)

nreps(simulation)
nvalues(simulation)

simulation1 <- subset_jags(simulation, rep = 1:2)
data_jags(simulation1)

nreps(simulation1)
simulation1 <- update_jags(simulation1, nreps = 2)
nreps(simulation1)

simulation2 <- subset_jags(simulation, value = 2, rep = 1)
simulation <- add_jags(simulation1, simulation2)
nreps(simulation)
values(simulation)
nvalues(simulation)

simulation <- update_jags(simulation1, nreps = 2, values = data.frame(nx = c(25)))
nreps(simulation)
values(simulation)
nvalues(simulation)
}
\seealso{
  \code{\link{jags_data_model}} and
  \code{\link{jaggernaut}}
}

