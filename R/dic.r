#' @export
dic <- function (object, ...) {
  UseMethod("dic", object)
}

dic.gsmcmc <- function (object) {
  
  deviance <- get_sims(object,"deviance")
  deviance <- as.vector(deviance)
  pD <- var(deviance) / 2
  Dbar <- mean(deviance)
  DIC <- Dbar + pD
  return (c(DIC = DIC, Dbar = Dbar, pD = pD))
}

#' @method dic janalysis
#' @S3method dic janalysis
#' @export
dic.janalysis <- function (object) {
  return (dic(object$mcmc))
}

