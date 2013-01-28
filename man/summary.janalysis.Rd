\name{summary.janalysis}
\alias{summary.janalysis}
\title{Summary for JAGS analysis}
\usage{
  summary.janalysis(object)
}
\arguments{
  \item{object}{a janalysis object}
}
\value{
  NULL
}
\description{
  Prints a summary of a JAGS analysis object parameter
  estimates for JAGS analysis
}
\examples{
model <- jmodel("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")
data <- data.frame(x = rpois(100,1))
analysis <- janalysis (model, data)
summary(analysis)
}

