
jags_mcmc <- function (mcmc, jags) {
  if (!inherits (mcmc,"list"))
    stop ("mcmc should be a list of mcarray objects")
  if(length(mcmc)<1)
    stop("mcmc should be a list of at least one mcarray object")
  if(class(mcmc[[1]])!='mcarray')
    stop("mcmc should be a list of at least one mcarray object")
  
  object <- list(mcmc = mcmc, jags = jags)
  class(object) <- "jags_mcmc"
  return (object)
}
