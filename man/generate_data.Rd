\name{generate_data}
\alias{generate_data}
\title{Generate dummy data}
\usage{
  generate_data(data, range = NULL, length.out = 30)
}
\arguments{
  \item{data}{the data frame of variables used to generate
  the dummy data frame}

  \item{range}{a character vector of the variables in data
  to represent by a sequence of values}

  \item{length.out}{the the number of values in a sequence}
}
\value{
  the dummy data frame
}
\description{
  Generates dummy data frame based on the variables in
  data. If a variable is specified in range then it is
  represented by a sequence of evenly spaced values from
  the minimum to the maximum of the observed values (the
  range) of length equal to length.out. If it is not
  specified in range then it is represented by the mean of
  the observed values. The returned data frame includes all
  combinations of the generated values.
}
\details{
  data <- data.frame(numeric = 1:10 + 0.1, integer = 1:10,
  factor = factor(1:10), date = as.Date("2000-01-01") +
  1:10, posixt = ISOdate(2000,1,1) + 1:10)

  generate_data (data) generate_data (data,range='numeric')
  generate_data (data,range=c('date','factor'))
}

