\name{estimates}
\alias{estimates}
\title{Calculate parameter estimates}
\usage{
  estimates(object, model = 1, parameters = "fixed")
}
\arguments{
  \item{object}{a \code{janalysis} object}

  \item{model}{an integer element specifying the model to
  select. If model = 0 then it selects the model with the
  lowest DIC.}

  \item{parameters}{a character vector of the parameters to
  calculate the estimates}
}
\value{
  a data.frame of the parameter estimates with the median
  estimate and lower and upper 95% credibility limits as
  well as the percent relative error and significance
}
\description{
  Calculates parameter estimates for a JAGS analysis
}
\examples{
# Poisson GLM analysis of peregrine breeding pairs (Kery & Schaub 2011 p.55-66)
mod <- model("
 model {
   alpha ~ dunif(-20, 20)
   beta1 ~ dunif(-10, 10)
   beta2 ~ dunif(-10, 10)
   beta3 ~ dunif(-10, 10)

   for (i in 1:nrow) {
     log(eCount[i]) <- alpha + beta1 * Year[i]
       + beta2 * Year[i]^2 + beta3 * Year[i]^3
     Count[i] ~ dpois(eCount[i])
   }
 }",
select = c("Count","Year*")
)
dat <- peregrine
dat$Count <- dat$Pairs
ana <- analysis (mod, dat)
estimates(ana)
}
\seealso{
  \link{analysis}
}

