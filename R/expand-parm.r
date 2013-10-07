
expand_parm <- function (object, ...) {
  UseMethod("expand_parm", object)
}

expand_parm.jagr_analysis_model <- function (object, parm = "all", ...) {

  stopifnot(is.jagr_analysis_model(object))
  stopifnot(is.character(parm) && is_length(parm) && is_defined(parm))
    
  parm <- sort(unique(parm))
  
  pars <- monitor(object)
  
  if("all" %in% parm || all(c("fixed","random") %in% parm))
    return (pars)
  
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
