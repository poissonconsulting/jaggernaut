analyses <- function (object) {
  UseMethod("analyses", object)
}

"analyses<-" <- function (object, value) {
  UseMethod("analyses<-", object)
}

analyses.jags_analysis <- function (object) {
  object$analyses
}

"analyses<-.jags_analysis" <- function (object, value) {
  stopifnot(is.list(value))
  stopifnot(all(unlist(lapply(value, is.jagr_analysis))))
  
  object$analyses <- value
  rename_analyses(object)
}
