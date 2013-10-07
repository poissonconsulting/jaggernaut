
analysis_model <- function (object, ...) {
  UseMethod("analysis_model", object)
}

analysis_model.jags_power_analysis <- function (object, ...) {
  return (object$analysis_model)
}
