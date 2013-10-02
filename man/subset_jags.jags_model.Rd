\name{subset_jags.jags_model}
\alias{subset_jags.jags_model}
\title{Subset a JAGS model}
\usage{
  \method{subset_jags}{jags_model} (object,
    model_number = NULL, ...)
}
\arguments{
  \item{object}{a jags_model object.}

  \item{model_number}{an integer element indicating the
  model(s) to select.}

  \item{...}{other arguments passed to generic function.}
}
\value{
  the subsetted jags_analysis object
}
\description{
  Subset a JAGS model object.
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

models <- add_jags(model1, model2, model1)
data <- data.frame(x = rpois(100,10))

analysis <- jags_analysis (models, data, mode = "demo")

summary(analysis)
summary(subset_jags(analysis))
summary(subset_jags(analysis, model_number = 2))
}
\seealso{
  \code{\link{subset_jags}} and \code{\link{jags_analysis}}
}

