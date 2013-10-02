
expand_parm <- function (object, parm = "all", ...) {

  stopifnot(is.jagr_analysis(object))
  stopifnot(is.character(parm) && is_length(parm) && is_defined(parm))
    
  parm <- unique(parm)
  
  pars <- monitor(object)
  
  if("all" %in% parm || all(c("fixed","random") %in% parm)) {
    return (pars)
  }
  
  add <- NULL
  
  if ("fixed" %in% parm) {
    add <- pars[!pars %in% random_variables(object)]
  } else if ("random" %in% parm) {
    add <- pars[pars %in% random_variables(object)]
  }
  pars <- pars[pars %in% parm]
  pars <- unique(c(pars,add))  
  
  return (pars)
}
