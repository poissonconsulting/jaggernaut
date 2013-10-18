
analyses <- function (object, ...) {
  UseMethod("analyses", object)
}

"analyses<-" <- function (object, value, ...) {
  UseMethod("analyses<-", object)
}

analyses.jags_analysis <- function (object, ...) {
  analyses <- object$analyses
  analyses <- name_object(analyses, "analysis")
  return (analyses)
}

analyses.jags_power_analysis <- function (object, ...) {
  analyses <- object$analyses
  analyses <- name_object(analyses,c("value","replicate"))
  return (analyses)
}

"analyses<-.jags_analysis" <- function (object, value, ...) {
  stopifnot(is.list(value))
  stopifnot(all(unlist(lapply(value, is.jagr_analysis))))
  
  object$dic <- NULL
  object$analyses <- value
  
  return (object)
}

"analyses<-.jags_power_analysis" <- function (object, value, ...) {  
  stopifnot(is.list(value) && is.list(value)[[1]])
  stopifnot(is.jagr_power_analysis(value[[1]][[1]]))
  stopifnot(is_scalar(unique(sapply(value,length))))
  stopifnot(length(value) == nvalues(object))
  
  object$analyses <- value
  
  return (object)
}
