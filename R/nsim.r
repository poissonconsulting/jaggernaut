
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

nsim.jagr_analysis <- function (object)
{
  if (!inherits(object,"jagr_analysis"))
    stop ("object should be class jagr_analysis")
  
  return (nchain(object)  * niter(object))
}