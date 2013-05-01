
nchain<- function (object, ...) {
  UseMethod("nchain", object)
}

nchain.mcarray <- function (object)
{
  if (!inherits(object,"mcarray"))
    stop ("object should be class mcarray")
  dim <- dim (object)
  return (as.integer(dim[length(dim)]))
}

nchain.mcmc.list <- function (object) {
  return (coda::nchain (object))
}

nchain.jags_mcmc <- function (object)
{
  if (!inherits(object,"jags_mcmc"))
    stop ("object should be class jags_mcmc")
  
  return (nchain (object$mcmc[[1]]))
}

nchain.jagr_analysis <- function (object)
{
  if (!inherits(object,"jagr_analysis"))
    stop ("object should be class jagr_analysis")
  
  return (nchain (object$mcmc))
}
