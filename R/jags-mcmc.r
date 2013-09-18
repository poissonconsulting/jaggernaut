
update_convergence_jags_mcmc <- function (object) {
  
  stopifnot(is.jags_mcmc(object))
  
  nsim <- nsim(object)
  
  mcmc <- as.mcmc.list (object)
  
  vars<-coda::varnames(mcmc)
  
  vars <- sort(vars)
  
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
  
  object$vars <- vars
  
  svars <- function (x) {
    x <- strsplit(x, split = "[", fixed = T)
    x <- delist(x)[1]
    return (x)
  }
  
  object$svars <- sapply(vars,svars)
  object$rhat <- round(rhat,2)
  object$ind <- round(ind,0)
  
  return (object)
}

jags_mcmc <- function (mcmc, jags) {
  if (!inherits (mcmc,"list"))
    stop ("mcmc should be a list of mcarray objects")
  if(length(mcmc)<1)
    stop("mcmc should be a list of at least one mcarray object")
  if(class(mcmc[[1]])!='mcarray')
    stop("mcmc should be a list of at least one mcarray object")
  
  object <- list(mcmc = mcmc, jags = jags, vars = NA, svars = NA, rhat = NA, ind = NA)
  class(object) <- "jags_mcmc"
  
  object <- update_convergence_jags_mcmc(object)

  return (object)
}
