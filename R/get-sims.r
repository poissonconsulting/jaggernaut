
get_sims <- function (object, pars = NULL) {
  if (!inherits(object,"gsmcmc"))
    stop("object should be of class gsmcmc")
  
  mcmc<-as.matrix(object)
    
  if(is.null(pars))
    return (mcmc)
    
  pars <- unlist(lapply(strsplit(pars,'[',fixed=T),FUN = function (x) x[1]))
  colnames <- unlist(lapply(strsplit(colnames(mcmc),'[',fixed=T),FUN = function (x) x[1]))

  bol <- !pars %in% colnames
  if(any(bol)) {
    warning(paste(pars[bol],"are not monitored parameters"))
  }
  bol<-rep(F, ncol(mcmc))
  for (par in pars) {
    bol <- bol | par == colnames                          
  }
  if(!any(bol)) {
    warning("none of the named parameters are monitored - returning deviance")
    return (mcmc[,"deviance",drop=F])    
  }
  return (mcmc[,bol,drop=F])
}
