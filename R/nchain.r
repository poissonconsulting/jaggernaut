
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

nchain.janalysis <- function (object)
{
  if (!inherits(object,"janalysis"))
    stop ("object should be class janalysis")
  
  return (nchain (object$mcmc))
}