\name{number_of_models.jags_model}
\alias{number_of_models.jags_model}
\title{Number of models in a JAGS model}
\usage{
  \method{number_of_models}{jags_model} (object, ...)
}
\arguments{
  \item{object}{a jags_model object}

  \item{...}{other arguments passed to generic function.}
}
\description{
  Gets the number of models in a JAGS model object
}
\examples{
model1 <- jags_model("
model {
 bLambda ~ dlnorm(0, 10^-2)
 for (i in 1:nrow) {
   x[i]~dpois(bLambda)
 }
}")

model2 <- jags_model("
model {
 bLambda ~ dnorm(0, 10^-2)
 sLambda ~ dunif(0, 5)
 for (i in 1:nrow) {
   x[i] ~ dnorm(bLambda, sLambda^-2)
 }
}")

model <- add_jags(model1, model2)
number_of_models(model)
}
\seealso{
  \code{\link{number_of_models}} and
  \code{\link{jags_model}}
}

