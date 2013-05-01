
nsim<- function (object, ...) {
  UseMethod("nsim", object)
}

nsim.mcarray <- function (object)
{
  if (!inherits(object,"mcarray"))
    stop ("object should be class mcarray")
  
  return (nchain(object)  * niter(object))
}

nsim.jags_mcmc <- function (object)
{
  if (!inherits(object,"jags_mcmc"))
    stop ("object should be class jags_mcmc")
  
  return (nchain(object)  * niter(object))
}

nsim.jagr_analysis <- function (object)
{
  if (!inherits(object,"jagr_analysis"))
    stop ("object should be class jagr_analysis")
  
  return (nchain(object)  * niter(object))
}
