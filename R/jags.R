
jags <- function (object) {
  UseMethod("jags", object)
}

"jags<-" <- function (object, value) {
  UseMethod("jags<-", object)
}

jags.jagr_chains <- function (object) {
  return (object$jags)
}

"jags<-.jagr_chains" <- function (object, value) {
  
  stopifnot(is_list_jags(value) || all.equal(value,list(NULL)))
  
  object$jags <- value
  
  return (object)
}
