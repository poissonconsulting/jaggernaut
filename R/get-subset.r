
get_subset_mcarray <- function (x, iter, chain)
{
  if (!inherits(x,"mcarray"))
    stop ("x must be of class mcarray")
  if (chain < 1)
    stop ("chain should be one or more")
  if (iter < 1)
    stop ("iter should be one or more")
  
  ndim <- length (dim(x))
  dnames <- dimnames(dim(x))
  
  commas <- paste0(rep(',',ndim - 2),collapse = "")   
  cmd <- paste0('x<-x[', commas ,iter, ',' ,chain, ',drop=F]')
  eval(parse(text = cmd))
  
  dimnames(x) <- dnames
  class(x) <- 'mcarray'
  
  return (x)
}

get_subset_gsmcmc <- function (x, iter, chain)
{
  if (!inherits(x,"gsmcmc"))
    stop ("x must be of class gsmcmc")
  if (chain < 1)
    stop ("chain should be one or more")
  if (iter < 1)
    stop ("iter should be one or more")
  if (chain > nchain (x))
    stop ("chain should not exceed the number of chains in x")
  if (iter > niter (x))
    stop ("iter should not exceed the number of iterations in x")
  
  x$mcmc <- lapply(x$mcmc,FUN = get_subset_mcarray,iter=iter,chain=chain)
    
  x$jags <- x$jags[chain]
  
  return (x)
}
