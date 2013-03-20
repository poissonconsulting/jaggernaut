\name{get_data}
\alias{get_data}
\title{Get data from a JAGS analysis object}
\usage{
  get_data(object)
}
\arguments{
  \item{object}{a JAGS analysis (janalysis) object}
}
\value{
  a data frame of the data used in the JAGS analysis
}
\description{
  Returns data from a JAGS analysis (janalysis) object
}
\examples{
model <- jmodel(
 model = "model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }"
)
data <- data.frame(x = rpois(100,1))
analysis <- janalysis (model, data)
get_data(analysis)
}

