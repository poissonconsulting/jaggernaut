
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

dic.jagr_analysis <- function (object) {
  return (dic(object$mcmc))
}


#' Calculate Deviance Information Criterion values for JAGS analysis
#'
#' Calculates Deviance Information Criterion values for JAGS analysis
#' 
#' @param object a janalysis object
#' @return a table of the Deviance Information Criterion values by model number
#' @method dic janalysis
#' @S3method dic janalysis
#' @export
#' @examples
#' model <- jmodel("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")
#' data <- data.frame(x = rpois(100,1))
#' analysis <- janalysis (model, data)
#' dic(analysis)
dic.janalysis <- function (object) {
  return (object$dic)
}

