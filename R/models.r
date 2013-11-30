
models <- function (object, ...) {
  UseMethod("models", object)
}

"models<-" <- function (object, value) {
  UseMethod("models<-", object)
}

models.jags_model <- function (object, ...) {
  models <- object$models
  models <- name_object(models, "Model")
  return (models)
}

"models<-.jags_model" <- function (object, value) {
  stopifnot(is.list(value))
  stopifnot(all(unlist(lapply(value, is.jagr_model))))
  
  object$models <- value
  return (object)
}
