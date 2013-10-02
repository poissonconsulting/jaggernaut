
nrep <- function (object, ...) {
  UseMethod("nrep", object)
}

nrep.jags_simulation <- function (object)
{
  return (as.integer(length(object$data[[1]])))
}
