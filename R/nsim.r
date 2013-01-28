
nsim<- function (object, ...) {
  UseMethod("nsim", object)
}

nsim.mcarray <- function (object)
{
  if (!inherits(object,"mcarray"))
    stop ("object should be class mcarray")
  
  return (nchain(object)  * niter(object))
}

nsim.gsmcmc <- function (object)
{
  if (!inherits(object,"gsmcmc"))
    stop ("object should be class gsmcmc")
  
  return (nchain(object)  * niter(object))
}

nsim.janalysis <- function (object)
{
  if (!inherits(object,"janalysis"))
    stop ("object should be class janalysis")
  
  return (nchain(object)  * niter(object))
}