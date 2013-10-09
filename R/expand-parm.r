
expand_parm <- function (object, ...) {
  UseMethod("expand_parm", object)
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
  } 
  if ("random" %in% parm) {
    random <- pars[pars %in% names(random_effects(object))]
  }
  pars <- pars[pars %in% parm]
  pars <- unique(c(pars,all,fixed,random))  
  
  return (pars)
}

expand_parm.jags_power_analysis <- function (object, parm = "all", ...) {
      
  model <- model(model(object))
  analysis <- analyses(object)[[1]][[1]]
  
  monitor(model) <- monitor(analysis)
  
  object <- c(model,analysis)
  
  class(object) <- c("jagr_analysis","jagr_analysis_model",
                     "jagr_model","jagr_power_analysis")
  
  return (expand_parm(object, parm = parm, ...))
}
