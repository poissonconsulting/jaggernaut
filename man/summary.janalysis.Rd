\name{summary.janalysis}
\alias{summary.janalysis}
\title{Summary of JAGS analysis (janalysis) object}
\usage{
  summary.janalysis(object)
}
\arguments{
  \item{object}{a janalysis object to summarise}
}
\value{
  a jsummary_analysis object #'
}
\description{
  Produces a summary of a JAGS analysis (janalysis) object
}
\examples{
model <- jmodel("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")
data <- data.frame(x = rpois(100,1))
analysis <- janalysis (model, data)
summary(analysis)
}
\seealso{
  \link[jaggernaut]{jmodel}, \link[jaggernaut]{janalysis}
}

