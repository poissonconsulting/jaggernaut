
calc_convergence <- function (object, ...) {
  UseMethod("calc_convergence", object)
}

calc_convergence.gsmcmc <- function (object, summarise = FALSE, parm = NULL) {

  nsim <- nsim(object)
  
  mcmc <- as.mcmc.list (object)
  
  vars<-coda::varnames(mcmc)
  
  bol<-rep(F, length(vars))
  
  svars <- sapply(vars,strsplit,split="[",fixed=T)
  for (i in 1:length(vars)) {
    bol[i] <- svars[[i]][1] %in% parm
  }
  
  vars<-vars[bol]
  rhat <- numeric()
  ind <- numeric()
  for (i in seq(along = vars)) {
    rhat[i] <- coda::gelman.diag(mcmc[,vars[i]])$psrf[1]
    ind[i] <- coda::effectiveSize(mcmc[,vars[i]])[1] / nsim * 100
  }
  convergence <- data.frame (
    convergence = round(rhat,2), 
    independence = round(ind,0),
    row.names = vars
  )
  
  convergence <- subset(convergence,!(is.na(convergence) & independence == 0))
  if (!summarise)
    return (convergence)
  
  independence <- min(convergence$independence)
  convergence <- max(convergence$convergence)
  convergence<-c(convergence,independence)
  names(convergence) <- c("convergence","independence")
  return (convergence)
}

calc_convergence.jagr_analysis <- function (object, summarise = TRUE, parm = parm) {

  return (calc_convergence (object$mcmc, summarise = summarise, parm = parm))
}

calc_convergence.jags_analysis <- function (object, summarise = TRUE, parm) {
  return (calc_convergence (top_model(object), summarise = summarise, parm = parm))
}


