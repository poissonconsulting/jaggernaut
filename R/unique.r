unique.Date <- function (x) {
  return (x[!duplicated(as.integer(x))])
}

unique.POSIXt <- function (x) {
  return (x[!duplicated(as.integer(x))])
}
