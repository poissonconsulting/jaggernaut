\name{aggregate.jags_analysis}
\alias{aggregate.jags_analysis}
\title{Aggregate jags_analysis iterations}
\usage{
\method{aggregate}{jags_analysis}(x, model_number = 1, data = dataset(x),
  aggregation_code = aggregation_code(x), ...)
}
\arguments{
  \item{x}{jags_analysis}

  \item{model_number}{integer scalar of model number}

  \item{aggregation_code}{string of aggregation_code to
  replace analyse's aggregation code}

  \item{...}{arguments passed to nowhere}
}
\description{
Runs aggregation code for jags_analysis to aggregate
iterations - used for posterior predictive checking.
}

