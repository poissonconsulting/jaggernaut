
dic_jags <- function (object, ...) {
  UseMethod("dic_jags", object)
}

"dic_jags<-" <- function (object, value, ...) {
  UseMethod("dic_jags<-", object)
}

dic_jags.jagr_chains <- function (object, ...) {

  deviance <- as.matrix(object,"deviance")
  deviance <- as.vector(deviance)
  pD <- var(deviance) / 2
  Dbar <- mean(deviance)
  DIC <- Dbar + pD
  
  return (c(DIC = DIC, Dbar = Dbar, pD = pD))
}

dic_jags.jagr_analysis <- function (object, ...) {
  return (dic_jags(as.jagr_chains(object), ...))
}

dic_jags_jagr_analysis <- function (object, ...) {
  stopifnot(is.jagr_analysis(object))
  return (dic_jags(object, ...))
}

dic_jags.jags_analysis <- function (object, ...) {
  return (object$dic)
}

"dic_jags<-.jags_analysis" <- function (object, value, ...) {

  object$dic <- value
  
  return (object)
}
