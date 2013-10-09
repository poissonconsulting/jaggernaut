
revise <- function (object, ...) {
  UseMethod("revise", object)
}

revise.jagr_chains <- function (object, ...) {
  
  mcmc <- as.mcmc.list (object)
  
  vars<-coda::varnames(mcmc)
  
  vars <- sort(vars)
  
  if(nchains(object) > 1) {
    rhat <- numeric()
    for (i in seq(along = vars)) {
      rhat[i] <- coda::gelman.diag(mcmc[,vars[i]])$psrf[1]
    }
  } else {
    rhat <- rep(NA,length(vars))
  }
  
  object$vars <- vars
  
  svars <- function (x) {
    x <- strsplit(x, split = "[", fixed = T)
    x <- delist(x)[1]
    return (x)
  }
  
  object$svars <- sapply(vars,svars)
  object$rhat <- round(rhat,2)
  
  return (object)
}

revise.jags_analysis <- function (object, ...) {
  
  analyses <- analyses(object)
    
  dic <- t(sapply(analyses,dic_jags_jagr_analysis))  
  rownames(dic) <- paste0("Model",1:nrow(dic))  
  dic <- dic[order(dic[,"DIC",drop=T]),]
  
  dic_jags(object) <- dic
  
  return (object)
}

revise.jags_power_analysis <- function (object, parm, level, ...) {
  
  power(object) <- NULL
  power <- power_jags(object, parm = parm, level = level)
  
#  power(object) <- power
#  level(object) <- level
  
  return (object)
}
