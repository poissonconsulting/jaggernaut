
nvalue<- function (object, ...) {
  UseMethod("nvalue", object)
}

nvalue.jags_simulation <- function (object)
{
  return (as.integer(nrow(object$values)))
}
