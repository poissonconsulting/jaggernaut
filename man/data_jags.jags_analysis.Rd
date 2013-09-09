\name{data_jags.jags_analysis}
\alias{data_jags.jags_analysis}
\title{Get dataset from a JAGS analysis}
\usage{
  \method{data_jags}{jags_analysis} (object, base = FALSE,
    ...)
}
\arguments{
  \item{object}{a jags_analysis object.}

  \item{based}{a logical element indicating xx.}

  \item{...}{other arguments passed to generic function.}
}
\value{
  a data.frame or list of the data used in the analysis
}
\description{
  Gets the dataset from a JAGS analysis object.
}
\examples{
model <- jags_model("
model {
 bLambda ~ dlnorm(0, 10^-2)
 for (i in 1:nrow) {
   x[i]~dpois(bLambda)
 }
}")

data <- data.frame(x = rpois(100,10))

analysis <- jags_analysis (model, data, mode = "demo")

data_jags(analysis)
data_jags(analysis, base = TRUE)
}
\seealso{
  \code{\link{data_jags}} and \code{\link{jags_analysis}}
}

