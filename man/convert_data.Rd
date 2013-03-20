\name{convert_data}
\alias{convert_data}
\title{Convert data}
\usage{
  convert_data(data, numericise = TRUE, centre = FALSE,
    standardise = FALSE, dat = NULL)
}
\arguments{
  \item{data}{the data frame on which the conversion is
  based}

  \item{numericise}{the variables to numericise}

  \item{centre}{the variables to centre}

  \item{standardise}{the variables to standardise}

  \item{dat}{the data frame to convert. If dat is NULL data
  is converted}
}
\value{
  the converted data frame
}
\description{
  Convert (by numericising, centring or standardising) the
  variables in a data frame into integer or numeric values
  ready for analysis by JAGS, OpenBUGS or WinBUGS.

  Numericising, centring and standardising represent
  increasing conversion of a variable. A variable is first
  converted into a integer value (numericising). Next the
  mean is substracted from all the values (centring).
  Finally all the values are divided by the standard
  deviation (standardising).

  Numericising has no effect on numeric and integer values
  and Date and POSIXt variables are numericised by
  converting to integer values, and subtracting the integer
  value of 1999-12-31 or 1999-12-31 23:59:59 GMT,
  respectively.

  Centring has no affect on factors and Date and POSIXt
  values are subtracted from the rounded mean. Similarly,
  standardising has no affect on factors and centred Date
  and POSIXt values are divided by their respective
  standard deviation.
}
\details{
  data <- data.frame(numeric = 1:10 + 0.1, integer = 1:10,
  factor = factor(1:10), date = as.Date("2000-01-01") +
  1:10, posixt = ISOdate(2000,1,1) + 1:10)
  convert_data(data) convert_data(data, centre = TRUE)
  convert_data(data, standardise = TRUE) convert_data(data,
  numericise = FALSE, standardise = "date")
  convert_data(data, numericise = FALSE, centre = "date",
  standardise = "date")
}

