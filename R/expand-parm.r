
expand_parm <- function (object, ...) {
  UseMethod("expand_parm", object)
}

expand_parm.jagr_chains <- function (x, parm = "all", ...) {
  
  if ("all" %in% parm)
    return (x$vars)
  
  return (x$vars[x$svars %in% parm])
}

expand_parm.jagr_analysis <- function (object, parm = "all", ...) {

  stopifnot(is.character(parm) && is_length(parm) && is_defined(parm))
    
  parm <- sort(unique(parm))
  
  pars <- monitor(chains(object))
  
  all <- NULL
  fixed <- NULL
  random <- NULL
  
  if("all" %in% parm) {
    all <- pars
  }
  if ("fixed" %in% parm) {
    fixed <- pars[!pars %in% names(random_effects(object))]
    fixed <- fixed[fixed != "deviance"]
  } 
  if ("random" %in% parm) {
    random <- pars[pars %in% names(random_effects(object))]
  }
  pars <- pars[pars %in% parm]
  pars <- sort(unique(c(all,fixed,random,pars)))
  
  return (pars)
}

expand_parm.jags_power_analysis <- function (object, parm = "all", indices = FALSE, ...) {
    
  model <- model(model(object))
  analysis <- analyses(object)[[1]][[1]]
  
  monitor(model) <- monitor(analysis)
  
  object <- c(model,analysis)
  
  class(object) <- c("jagr_analysis","jagr_analysis_model",
                     "jagr_model","jagr_power_analysis")
  
  parm <- expand_parm(object, parm = parm, ...)
  
  if(!indices)
    return (parm)
  
  parm <- expand_parm(chains(analysis), parm = parm, ...)
  
  return (parm)
}
