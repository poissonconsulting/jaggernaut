
get_parm<- function (object, ...) {
  UseMethod("get_parm", object)
}

get_parm.jagr_analysis <- function (object, parm = "fixed") {
  
  stopifnot(is.jagr_analysis(object))  
  stopifnot(is.character(parm))
  stopifnot(length(parm) > 0)
  
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

get_parm.jags_analysis <- function (object, parm = "fixed", model_number = 1) {

  stopifnot(is.jags_analysis(object))
  stopifnot(is.character(parm))
  stopifnot(length(parm) > 0)
  
  object <- subset_jags(object, model = model_number)
  
  return (get_parm (as.jagr_analysis(object), parm = parm))
}
