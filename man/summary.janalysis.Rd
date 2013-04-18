\name{summary.janalysis}
\alias{summary.janalysis}
\title{Summary of JAGS analysis}
\usage{
  \method{summary}{janalysis} (object, ...)
}
\arguments{
  \item{object}{a janalysis object to summarise}

  \item{...}{further arguments to pass to or from other
  methods.}
}
\value{
  a jsummary_analysis object
}
\description{
  Produces a summary of a JAGS analysis
}
\examples{
mod <- model("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")
dat <- data.frame(x = rpois(100,1))
ana <- analysis (mod, dat)
summary(ana)
}
\seealso{
  \code{\link{analysis}}
}

