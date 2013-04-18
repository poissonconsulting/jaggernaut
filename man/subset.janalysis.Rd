\name{subset.janalysis}
\alias{subset.janalysis}
\title{Subset a JAGS analysis}
\usage{
  \method{subset}{janalysis} (x, model = 0, ...)
}
\arguments{
  \item{x}{a janalysis object to subset}

  \item{model}{an integer vector specifying the model to
  select. If model = 0 then it selects the model with the
  lowest DIC.}

  \item{...}{further arguments to pass to or from other
  methods.}
}
\value{
  a janalysis object
}
\description{
  Subsets a JAGS analysis.
}
\examples{
mod1 <- model("model {
 bLambda ~ dunif(0,10)
 for (i in 1:nrow) {
   x[i] ~ dpois(bLambda)
 }
}")
mod2 <- model("model {
 bLambda ~ dunif(0,10)
 r ~ dgamma(0.1, 0.1)
 for (i in 1:nrow) {
   u[i] ~ dgamma(r,r)
   x[i] ~ dpois(bLambda * u[i])
 }
}")
mods <- list(mod1, mod2)
dat <- data.frame(x = rpois(100,1))
ana <- analysis (mods, dat)
summary(ana)
summary(subset(ana,model = 0))
summary(subset(ana,model = 2))
}

