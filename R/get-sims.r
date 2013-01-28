
get_sims <- function (object, pars = NULL) {
  if (!inherits(object,"gsmcmc"))
    stop("object should be of class gsmcmc")
  
  mcmc<-as.matrix(object)
    
  if(is.null(pars))
    return (mcmc)
  
  bol<-rep(F, ncol(mcmc))
  
  pars <- unlist(lapply(strsplit(pars,'[',fixed=T),FUN = function (x) x[1]))
  colnames <- unlist(lapply(strsplit(colnames(mcmc),'[',fixed=T),FUN = function (x) x[1]))

  for (par in pars) {
    bol <- bol | par == colnames                          
  }
  return (mcmc[,bol,drop=F])
}
