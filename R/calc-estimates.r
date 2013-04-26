
calc_estimates<- function (object, ...) {
  UseMethod("calc_estimates", object)
}

calc_estimates.matrix <- function(object) {
  object <- as.matrix (object)
  
  p<-function (x) {
    x<-sum(as.integer(x>=0))/length(x)
    x<-round(x,4)
    return (min(x,1-x)*2)
  }
  pre<-function (x) {
    level <- opts_jagr("level")
    lower <- (1 - level) / 2
    upper <- level + lower
    q<-quantile(x,c(lower,0.5,upper),na.rm=T)
    x<-round((q[3]-q[1]) / 2 / q[2] * 100)
    x<-abs(round(x,0))
    return (x)
  }
  fun<-function (x) {
    level <- opts_jagr("level")
    lower <- (1 - level) / 2
    upper <- level + lower
    return (c(quantile(x,c(0.5,lower,upper),na.rm=T),pre(x),p(x)))
  }
    
  estimates<-data.frame(t(apply(object,MARGIN=2,FUN=fun)))
  rownames(estimates)<-colnames(object)
  colnames(estimates)<-c('estimate','lower','upper','error','significance')
  return (estimates)
}


calc_estimates.gsmcmc <- function (object, parm = NULL) {
  return (calc_estimates (get_sims (object, parm)))
}

calc_estimates.jagr_analysis <- function (object, parm) {
  
  if(!is.jagr_analysis(object))
    stop ("object should be class jagr_analysis")
  
  if (!is.character(parm))
    stop ("parm must be a character vector")
  
  return (calc_estimates (object$mcmc, parm = parm))
}

calc_estimates.jags_analysis <- function (object, parm) {
  
  return (calc_estimates(top_model(object), parm = parm))
}

calc_estimates.gssimulation <- function (object, pars = NULL) {
  
  if(!is.gssimulation(object))
    stop ("object should be class gssimulation")
  
  return (calc_estimates (object$mcmc, pars = pars))
}

calc_estimates.gspower <- function (object, pars = "fixed") {
    
    if(!is.gspower(object))
      stop ("object should be class gspower")
    
    if (!(is.null(pars) || is.character(pars)))
      stop ("pars must be NULL or a character vector")
    
    if (is.null(pars) || (length(pars) == 1 && pars %in% c("all","fixed","random"))) {
      pars <- get_pars (object$analyses[[1]][[1]]$block, type = pars)
    } else {
      monitor <- object$analyses[[1]][[1]]$block$monitor

      bol <- pars %in% monitor
      if (any (!bol))
        warning(paste(pars[!bol],"were not monitored in analyses"))
      pars <- pars[bol]      
    }

    mcmc <- object$analyses[[1]][[1]]$mcmc
    ests <- calc_estimates (mcmc, pars = pars)
    ppars <- pars
    
    pars <- rownames(ests)
    npars <- length(pars)
    
    mat <- matrix(NA, nrow = npars, ncol = object$nreps)
    
    estimates <- list()
    
    for (i in 1:object$nvalues) {
      est <- mat
      low <- mat
      upp <- mat
      
      for (j in 1:object$nreps) {

        mcmc <- object$analyses[[i]][[j]]$mcmc
        ests <- calc_estimates (mcmc, pars = ppars)
        
        for (k in 1:npars) {
          est[k,j] <- ests[pars[k],"estimate"]
          low[k,j] <- ests[pars[k],"lower"]
          upp[k,j] <- ests[pars[k],"upper"]
        }
      }

      est <- apply(est, 1, quantile, c(0.5)) 
      low <- apply(low, 1, quantile, c(0.025))
      upp <- apply(upp, 1, quantile, c(0.975))
                  
      estimates[[i]] <- data.frame(estimate = est, lower = low, upper = upp)
      rownames (estimates[[i]]) <- pars
    }
    names(estimates) <- rownames(object$values)

    return (estimates)
}
