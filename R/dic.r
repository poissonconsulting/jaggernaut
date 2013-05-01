
DIC_jags_mcmc <- function (object) {
  
  deviance <- get_sims(object,"deviance")
  deviance <- as.vector(deviance)
  pD <- var(deviance) / 2
  Dbar <- mean(deviance)
  DIC <- Dbar + pD
  return (c(DIC = DIC, Dbar = Dbar, pD = pD))
}

DIC_jagr_analysis <- function (object) {
  return (DIC_jags_mcmc(object$mcmc))
}

DIC <- function (object) {
  return (object$dic)
}

