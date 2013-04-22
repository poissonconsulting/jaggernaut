\name{dataset}
\alias{dataset}
\title{Get data from a JAGS analysis}
\usage{
  dataset(object, base = FALSE)
}
\arguments{
  \item{object}{a jags_analysis object}

  \item{base}{a logical scalar indicating whether to return
  the base values}
}
\value{
  a data frame of the data used in the analysis or the
  corresponding base values
}
\description{
  By default returns the input data set from a JAGS
  analysis or if \code{base = TRUE} generates a data set
  with one row specify the #' \emph{base} values for each
  of the variables. For further information on the use of
  base values see the \code{predict.jags_analysis}
  function.
}
\seealso{
  \link{jags_analysis} and \link{predict.jags_analysis}
}

