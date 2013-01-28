\name{plot.janalysis}
\alias{plot.janalysis}
\title{Plot JAGS analysis}
\usage{
  plot.janalysis(x)
}
\arguments{
  \item{object}{a JAGS analysis object}
}
\value{
  NULL
}
\description{
  Produces trace plots for a JAGS analysis object
}
\examples{
model <- jmodel("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")
data <- data.frame(x = rpois(100,1))
analysis <- janalysis (model, data)
plot(analysis)
}

