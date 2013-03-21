
DIC_gsmcmc <- function (object) {
  
  deviance <- get_sims(object,"deviance")
  deviance <- as.vector(deviance)
  pD <- var(deviance) / 2
  Dbar <- mean(deviance)
  DIC <- Dbar + pD
  return (c(DIC = DIC, Dbar = Dbar, pD = pD))
}

DIC_jagr_analysis <- function (object) {
  return (DIC_gsmcmc(object$mcmc))
}

#' @title Calculate Deviance Information Criterion values for JAGS analysis
#'
#' @description
#' Calculates Deviance Information Criterion values for JAGS analysis
#' 
#' @param object a janalysis object
#' @return a table of the Deviance Information Criterion values by model number
#' @details
#' model <- jmodel("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")
#' data <- data.frame(x = rpois(100,1))
#' analysis <- janalysis (model, data)
#' DIC(analysis)
DIC <- function (object) {
  return (object$dic)
}

