
expand_parm <- function (object, parm = "all") {
  
  stopifnot(is.character(parm) && is_length(parm) && is_defined(parm))
  
  model <- object$model
  
  parm <- unique(parm)
  
  pars <- model$monitor
  
  if("all" %in% parm || all(c("fixed","random") %in% parm)) {
    return (pars)
  }
  
  add <- NULL
  
  if ("fixed" %in% parm) {
    add <- pars[!pars %in% names(model$random)]
  } else if ("random" %in% parm) {
    add <- pars[pars %in% names(model$random)]
  }
  pars <- pars[pars %in% parm]
  pars <- unique(c(pars,add))  
  
  return (pars)
}
