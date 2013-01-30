
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

nchain.gsmcmc <- function (object)
{
  if (!inherits(object,"gsmcmc"))
    stop ("object should be class gsmcmc")
  
  return (nchain (object$mcmc[[1]]))
}

nchain.jagr_analysis <- function (object)
{
  if (!inherits(object,"jagr_analysis"))
    stop ("object should be class jagr_analysis")
  
  return (nchain (object$mcmc))
}