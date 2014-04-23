\name{jags_discrepancies}
\alias{jags_discrepancies}
\title{JAGS analysis discrepancies}
\usage{
jags_discrepancies(object, model_number = 1, aggregation_code = NULL)
}
\arguments{
  \item{object}{jags_analysis}

  \item{model_number}{integer scalar of model to get
  discrepancies for}

  \item{aggregation_code}{string of JAGS model code
  defining paired posterior predictive checking aggregation
  parameters parameters}
}
\description{
Uses aggregation code parameters with just two levels to
get an object of class jags_discrepancies that can be used
to plot visuals of discrepancies or calculate bayesian
p-values using coef or extract data frame using dataset for
own plotting.
}
\examples{
model1 <- jags_model(
" model {
   sVolume ~ dunif(0, 100)
bIntercept ~ dnorm (0, 100^-2)
bGirth ~ dnorm(0, 100^-2)
for (i in 1:length(Volume)) {
eVolume[i] <- bIntercept + bGirth * Girth[i]
Volume[i] ~ dnorm(eVolume[i], sVolume^-2)
}
}",
aggregation_code = "model {
for (i in 1:length(Volume)) {
prediction[i] <- bIntercept + bGirth * Girth[i]
E[i] <- pow(Volume[i] - prediction[i], 2) / prediction[i]
newVolume[i] ~ dnorm(prediction[i], sVolume^-2)
E2[i] <- pow(newVolume[i] - prediction[i], 2) / prediction[i]
}
EE[1] <- sum(E)
EE[2] <- sum(E2)
}",
select = c("Volume","Girth")
 )

 models <- combine(model1)

 data <- trees

 analysis <- jags_analysis(models, data = data, mode = "test")

 discrepancies <- jags_discrepancies(analysis)

 dataset(discrepancies)
 coef(discrepancies)
 \dontrun{
 plot(discrepancies)
 }
}

