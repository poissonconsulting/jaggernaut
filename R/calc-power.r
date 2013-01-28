
calc_power <- function (object, pars = NULL) {
  
  if(!is.gspower(object))
    stop ("object should be class gspower")
  
  if (!(is.null(pars) || (is.numeric(pars) && !is.null(pars))))
    stop ("pars must be NULL or a named numeric vector")
    
  monitor <- object$analyses[[1]][[1]]$block$monitor
  
  if(is.null(pars)) {
    pars <- get_pars(object$analyses[[1]][[1]]$block)
    pname <- pars
    pars <- rep(0,length(pars))
    names(pars) <- pname
  }
  
  bol <- names(pars) %in% monitor
  if (any (!bol)) 
    warning(paste(names(pars)[!bol],"were not monitored in analyses"))
  pars <- pars[bol]
  
  pars <- pars[order(names(pars))]
  
  object$parnames = names(pars)
  object$parvalues = pars
  names(object$parvalues) <- NULL
  
  est <- calc_estimates (object$analyses[[1]][[1]]$mcmc, pars = object$parnames)
  pars <- rownames(est)
  npars <- length(pars)
    
  power <- matrix(NA, nrow = object$nvalues, ncol = npars)
  rownames(power) <- rownames(object$values)
  colnames(power) <- pars
  
  for (i in 1:object$nvalues) {
    out <- rep(0, npars)
    for (j in 1:object$nreps) {
      est <- calc_estimates (object$analyses[[i]][[j]]$mcmc, pars = object$parnames)
      for (k in 1:npars) {
        low <- est[pars[k],"lower"]
        upp <- est[pars[k],"upper"]
        
        bol <- pars[k] == object$parnames
        if(any(bol)) {
          stopifnot(length(bol[bol]) == 1)
          point <- object$parvalues[bol]
        } else {
          bol <- sapply(strsplit(pars[k],"\\["),function (x) x[1]) == object$parnames
          stopifnot(length(bol[bol]) == 1)
          point <- object$parvalues[bol]          
        }
        
        if(!(point >= low && point <= upp))
          out[k] <- out[k] + 1
      }
      power[i,] <- out / object$nreps
    }
  }
  
  object$power <- cbind(object$values, as.data.frame(power))
  return (object)
}
