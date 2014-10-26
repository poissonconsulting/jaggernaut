
analyses <- function (object) {
  UseMethod("analyses", object)
}

"analyses<-" <- function (object, value) {
  UseMethod("analyses<-", object)
}

analyses.jags_analysis <- function (object) {
  analyses <- object$analyses
  analyses <- name_object(analyses, "analysis")
  return (analyses)
}

"analyses<-.jags_analysis" <- function (object, value) {
  stopifnot(is.list(value))
  stopifnot(all(unlist(lapply(value, is.jagr_analysis))))
  
  object$dic <- NULL
  object$analyses <- value
  
  return (object)
}
