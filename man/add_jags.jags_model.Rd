\name{add_jags.jags_model}
\alias{add_jags.jags_model}
\title{Add JAGS models}
\usage{
  \method{add_jags}{jags_model} (object, object2, ...)
}
\arguments{
  \item{object}{a jags_model.}

  \item{object2}{a second jags_model.}

  \item{...}{additional jags_models to add to object.}
}
\value{
  a jags_model object
}
\description{
  Adds two or more JAGS models.
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

models <- add_jags(model1, model2)
number_of_models(models)
}
\seealso{
  \code{\link{add_jags}} and \code{\link{jags_model}}
}

