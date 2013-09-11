
calc_convergence_jags_mcmc <- function (object) {

  nsim <- nsim(object)
  
  mcmc <- as.mcmc.list (object)
  
  vars<-coda::varnames(mcmc)
  
  if(nchain(object) > 1) {
    rhat <- numeric()
    ind <- numeric()
    for (i in seq(along = vars)) {
      rhat[i] <- coda::gelman.diag(mcmc[,vars[i]])$psrf[1]
      ind[i] <- coda::effectiveSize(mcmc[,vars[i]])[1] / nsim * 100
    }
  } else {
    rhat <- rep(NA,length(vars))
    ind <- rep(NA,length(vars))
  }
  convergence <- data.frame (
    convergence = round(rhat,2), 
    independence = round(ind,0),
    row.names = vars
  )

  return (convergence)
}

jags_mcmc <- function (mcmc, jags) {
  if (!inherits (mcmc,"list"))
    stop ("mcmc should be a list of mcarray objects")
  if(length(mcmc)<1)
    stop("mcmc should be a list of at least one mcarray object")
  if(class(mcmc[[1]])!='mcarray')
    stop("mcmc should be a list of at least one mcarray object")
  
  object <- list(mcmc = mcmc, jags = jags)
  class(object) <- "jags_mcmc"
  
  conv <- calc_convergence_jags_mcmc(object)
  
  rhat <- conv[,"convergence",drop=FALSE]
  ind <- conv[,"independence",drop=FALSE]
  
  object$rhat <- rhat
  object$ind <- ind

  return (object)
}
