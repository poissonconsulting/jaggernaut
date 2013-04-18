\name{dataset}
\alias{dataset}
\title{Get data from a JAGS analysis}
\usage{
  dataset(object, base = FALSE)
}
\arguments{
  \item{object}{a janalysis object}

  \item{base}{a logical scalar indicating whether to return
  the base values}
}
\value{
  a data frame of the data used in the analysis or the
  corresponding base values
}
\description{
  Returns the input data set from a JAGS analysis or
  generates a data set with one row specify the base values
  for each of the variables. For further information on the
  use of base values see the \code{derived} function.
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
dataset(ana)
dataset(ana, base = TRUE)
}
\seealso{
  \link{analysis}, \link{derived}
}

