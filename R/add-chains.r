
add_chains <- function (object, chain) {
  UseMethod("add_chains", object)
}

add_chains.mcarray <- function (object, chain) {
  if (!inherits (object, "mcarray"))
    stop ("object should be class mcarray")
  if (!inherits (chain, "mcarray"))
    stop ("chain should be class mcarray")
  if (niter (object) != niter (chain))
    stop ("object and chain should have the same number of iterations")
  
  dimobj <- dim (object)
  dimchain <- dim (chain)
  dnames <- names(dim (object))
  
  if (!identical(dimobj[-length(dimobj)],dimchain[-length(dimchain)]))
    stop ("object and chain should have the same dimensions (except chains)")
  
  class(object)<-"array"
  class(chain)<-"array"
  object <- abind (object,chain,along=length(dimobj))
  
  names(dim(object)) <- dnames
  class(object)<-"mcarray"
  
  return (object)
}

add_chains.list <- function (object, chain) {
  if (!inherits (object, "list"))
    stop ("object should be class list")
  if (!inherits (chain, "list"))
    stop ("chain should be class list")
  if (length (object)!=length(chain))
    stop ("object and chain should be the same length")
  if (!identical(names(object),names(chain)))
    stop ("object and chain should have the same names")
  
  for (i in seq(along = object))
    object[[i]] <- add_chains(object[[i]],chain[[i]])
  
  return (object)
}

add_chains.jags_mcmc <- function (object, chain) {
  
  object$jags <- c(object$jags, chain$jags)
  
  object$mcmc <- add_chains (object$mcmc, chain$mcmc)
  return (object)
}

add_chains_jags_mcmc <- function (object, chain) {

  object$jags <- c(object$jags, chain$jags)
  object$mcmc <- add_chains (object$mcmc, chain$mcmc)
  return (object)
}
