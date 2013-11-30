
data_model <- function (object, ...) {
  UseMethod("data_model", object)
}

"data_model<-" <- function (object, value) {
  UseMethod("data_model<-", object)
}

data_model.jags_simulation <- function (object, ...) {
  return (object$data_model)
}

"data_model<-.jags_simulation" <- function (object, value) {
  stopifnot(is.jags_data_model(value))
  
  object$data_model <- value
  return (object)
}
