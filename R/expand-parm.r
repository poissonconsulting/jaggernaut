
expand_parm <- function (object, parm, ...) {
  UseMethod("expand_parm", object)
}

expand_parm.jags_model <- function (object, parm = "all", ...) {
  
  stopifnot(is.character(parm) && is_length(parm) && is_defined(parm))
      
  parm <- unique(parm)
  
  pars <- monitor(object)
  
  if("all" %in% parm || all(c("fixed","random") %in% parm)) {
    return (pars)
  }
  
  add <- NULL
  
  if ("fixed" %in% parm) {
    add <- pars[!pars %in% names(random_effects(object))]
  } else if ("random" %in% parm) {
    add <- pars[pars %in% names(random_effects(object))]
  }
  pars <- pars[pars %in% parm]
  pars <- unique(c(pars,add))  
  
  return (pars)
}

expand_parm.jagr_analysis <- function (object, parm = "all", ...) {

  stopifnot(is.character(parm) && is_length(parm) && is_defined(parm))
  
  return (expand_parm(as.jags_model(object), parm = parm, ...))
}

expand_parm_jagr_analysis <- function (object, parm = "all", ...) {
  
  stopifnot(is.jagr_analysis(object))
  
  return (expand_parm(object, parm = parm, ...))
}

expand_parm.jags_analysis <- function (object, parm = "all", ...) {
  
  stopifnot(is.character(parm) && is_length(parm) && is_defined(parm))
  
  object <- as.jagr_analysis(object)
    
  if(is.jagr_analysis(object))
    return (expand_parm(object, parm = parm, ...))
    
  parm <- lapply(object, expand_parm_jagr_analysis, parm = parm, ...)
  
  return (parm)
}
