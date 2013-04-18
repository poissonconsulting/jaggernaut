\name{convergence}
\alias{convergence}
\title{Calculate convergence values}
\usage{
  convergence(object, model = 1, parameters = "fixed")
}
\arguments{
  \item{object}{a janalysis object}

  \item{model}{an integer element specifying the model to
  select. If model = 0 then it selects the model with the
  lowest DIC.}

  \item{parameters}{a character vector specifying the
  parameters for which to calculate the convergence}
}
\value{
  a data.frame of the parameters with their convergence
  (R-hat) values
}
\description{
  Calculates convergence (R-hat) values for the parameters
  in a JAGS analysis
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
convergence(ana)
}
\seealso{
  \code{\link{analysis}}
}

