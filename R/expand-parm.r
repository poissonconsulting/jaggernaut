
expand_parm <- function (object, ...) {
  UseMethod("expand_parm", object)
}

expand_parm.jagr_chains <- function (object, parm, ...) {
  
  mcmc <- as.mcmc.list (object)
  
  vars <- coda::varnames(mcmc)
  
  vars <- sort(vars)
  
  get_svars <- function (x) {
    x <- strsplit(x, split = "[", fixed = T)
    x <- delist(x)[1]
    return (x)
  }
  
  svars <- sapply(vars, get_svars)
  
  all <- NULL
  fixed <- NULL
  random <- NULL
  
  if("all" %in% parm) {
    all <- svars
  }
  if ("fixed" %in% parm) {
    fixed <- svars[!svars %in% random(object)]
    fixed <- fixed[fixed != "deviance"]
  } 
  if ("random" %in% parm) {
    random <- svars[svars %in% random(object)]
  }
  pars <- svars[svars %in% parm]
  
  pars <- sort(unique(c(all,fixed,random,pars)))
  
  return (vars[svars %in% pars])
}
