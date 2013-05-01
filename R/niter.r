
niter<- function (object, ...) {
  UseMethod("niter", object)
}

niter.mcarray <- function (object)
{
  if (!inherits(object,"mcarray"))
    stop ("object should be class mcarray")
  dim <- dim (object)
  return (as.integer(dim[length(dim)-1]))
}

niter.mcmc.list <- function (object) {
  return (coda::niter (object))
}

niter.jags_mcmc <- function (object)
{
  if (!inherits(object,"jags_mcmc"))
    stop ("object should be class jags_mcmc")
  
  return (niter (object$mcmc[[1]]))
}

niter.jagr_analysis <- function (object)
{
  if (!inherits(object,"jagr_analysis"))
    stop ("object should be class jagr_analysis")
  
  return (niter (object$mcmc))
}
