
update.jagr_analysis <- function (object, quiet = F)
{  
  n.chain <- nchain(object)
  n.sim <- object$iterations
  n.thin <- max(1, floor(n.chain * n.sim / nsim(object)))

  monitor <- object$model$monitor
  
  jags <- object$mcmc$jags
  
  parallel <- length(jags) > 1
  
  ptm <- proc.time()
  
  if (parallel) {
    doMC::registerDoMC(cores=n.chain)   
    i <- 1 # hack to prevent warning on package check
    mcmc <- foreach::foreach(i = 1:n.chain, .combine = add_chains_jags_mcmc) %dopar% {
      update_jags(
        jags = jags[[i]], monitor = monitor, n.sim = n.sim, n.thin = n.thin, 
        quiet = quiet, recompile = T
      )
    }
  } else {    
    mcmc <- update_jags (
      jags = jags[[1]], monitor = monitor, n.sim = n.sim, n.thin = n.thin, 
      quiet = quiet, recompile = F
    )
  }
  object$mcmc <- mcmc
  object$iterations <- object$iterations * 2
  object$time <- object$time + ((proc.time () - ptm)[3]) / (60 * 60)

  return (object)
}

update_jags <- function (jags, monitor, n.sim, n.thin, quiet, recompile)
{
  n.sim <- as.integer(n.sim)
  n.thin <- as.integer(n.thin)
  
  if (quiet) {
    options(jags.pb = "none")
  } else {
    options(jags.pb = "text")
  }
  
  if (recompile)
    jags$recompile()
  
  mcmc <- jags.samples(
    model = jags, variable.names = monitor, n.iter = n.sim, thin = n.thin
  )
  
  mcmc <- jags_mcmc(mcmc=mcmc,jags=list(jags))
  return (mcmc)
}
