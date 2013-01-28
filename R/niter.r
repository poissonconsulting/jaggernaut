
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

niter.gsmcmc <- function (object)
{
  if (!inherits(object,"gsmcmc"))
    stop ("object should be class gsmcmc")
  
  return (niter (object$mcmc[[1]]))
}

niter.janalysis <- function (object)
{
  if (!inherits(object,"janalysis"))
    stop ("object should be class janalysis")
  
  return (niter (object$mcmc))
}