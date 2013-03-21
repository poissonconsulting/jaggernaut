\name{subset.janalysis}
\alias{subset.janalysis}
\title{Subset a JAGS analysis}
\usage{
  subset.janalysis(object, model = 0)
}
\arguments{
  \item{object}{a janalysis object to subset}

  \item{model}{an integer vector specifying the models to
  select}
}
\value{
  a janalysis object
}
\description{
  Subsets a JAGS analysis.
}
\examples{
model1 <- jmodel("model {
 bLambda ~ dunif(0,10)
 for (i in 1:nrow) {
   x[i] ~ dpois(bLambda)
 }
}")
model2 <- jmodel("model {
 bLambda ~ dunif(0,10)
 r ~ dgamma(0.1, 0.1)
 for (i in 1:nrow) {
   u[i] ~ dgamma(r,r)
   x[i] ~ dpois(bLambda * u[i])
 }
}")#' models <- list(model1, model2)
data <- data.frame(x = rpois(100,1))
analysis <- janalysis (models, data)
summary(analysis)
summary(subset(analysis,model = 0))
summary(subset(analysis,model = 2))
}

