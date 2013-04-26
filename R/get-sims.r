
get_sims <- function (object, parm = NULL) {
  if (!inherits(object,"gsmcmc"))
    stop("object should be of class gsmcmc")
  
  mcmc<-as.matrix(object)
    
  if(is.null(parm))
    return (mcmc)
    
  parm <- unlist(lapply(strsplit(parm,'[',fixed=T),FUN = function (x) x[1]))
  colnames <- unlist(lapply(strsplit(colnames(mcmc),'[',fixed=T),FUN = function (x) x[1]))

  bol <- !parm %in% colnames
  if(any(bol)) {
    warning(paste(parm[bol],"are not monitored parameters"))
  }
  bol<-rep(F, ncol(mcmc))
  for (par in parm) {
    bol <- bol | par == colnames                          
  }
  if(!any(bol)) {
    warning("none of the named parameters are monitored - returning deviance")
    return (mcmc[,"deviance",drop=F])    
  }
  return (mcmc[,bol,drop=F])
}
