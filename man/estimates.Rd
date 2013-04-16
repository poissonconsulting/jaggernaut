\name{estimates}
\alias{estimates}
\title{Calculate parameter estimates for a JAGS analysis}
\usage{
  estimates(object, parameters = "fixed", average = FALSE)
}
\arguments{
  \item{object}{a \code{\link{janalysis}} object}

  \item{parameters}{a character vector of the parameters to
  calculate the estimates}

  \item{average}{a logical scalar indicating whether to
  model average - not yet implemented}
}
\value{
  a data.frame of the parameter estimates with the median
  estimate and lower and upper 95% credibility limits as
  well as the percent relative error and significance
}
\description{
  Calculate parameter estimates for a JAGS analysis
}
\examples{
# Poisson GLM analysis of peregrine breeding pairs (Kery & Schaub 2011 p.55-66)
model <- jmodel("
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
data <- peregrine
data$Count <- data$Pairs
analysis <- janalysis (model, data)
estimates(analysis)
}
\seealso{
  \link[jaggernaut]{jmodel}, \link[jaggernaut]{janalysis}
}

