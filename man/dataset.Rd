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
  Gets data from a JAGS analysis
}
\examples{
model <- jmodel("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")
data <- data.frame(x = rpois(100,1))
analysis <- janalysis (model, data)
dataset(analysis)
dataset(analysis, base = TRUE)
}
\seealso{
  \link[jaggernaut]{jmodel}, \link[jaggernaut]{janalysis}
}

