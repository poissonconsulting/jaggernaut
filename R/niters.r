
#' @title Number of MCMC iterations used to generate a JAGS object
#'
#' @description 
#' Gets the number of MCMC iterations used to generate a JAGS object
#'   
#' @param object a JAGS object
#' @return an integer element indicating the number of MCMC iterations used to generate object
#' @export
niters <- function (object) {
  UseMethod("niters", object)
}


"niters<-" <- function (object, value) {
  UseMethod("niters<-", object)
}

niters.jagr_power_analysis <- function (object) {
  return (object$niters)
}

niters_jagr_power_analysis <- function (object) {
  stopifnot(is.jagr_power_analysis(object))
  return (niters (object))
}

#' @method niters jags_analysis
#' @export
niters.jags_analysis <- function (object) {
  if(is_one_model(object))
    return (niters(analysis(object)))
  
  analyses <- analyses(object)
  analyses <- lapply(analyses, niters_jagr_power_analysis)
  analyses <- name_object(analyses, "Model")
  return (analyses)
}

niters_jags_analysis <- function (object) {
  stopifnot(is.jags_analysis(object))
  return (niters (object))
}

"niters<-.jagr_power_analysis" <- function (object, value) {
  
  stopifnot(is_integer_scalar(value))
  stopifnot(is_bounded(value, min = 1))

  value <- as.integer(value)
  
  object$niters <- value
  
  return (object)
}
