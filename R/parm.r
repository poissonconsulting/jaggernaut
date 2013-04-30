
parm<- function (object, ...) {
  UseMethod("parm", object)
}

parm.jagr_analysis <- function (object, parm = "fixed") {
  
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

parm.jags_analysis <- function (object, parm = "fixed", model_number = 1) {

  stopifnot(is.jags_analysis(object))
  stopifnot(is.character(parm))
  stopifnot(length(parm) > 0)
  
  object <- subset(object, model_number = model_number)
  
  return (parm (object$analyses[[1]], parm = parm))
}
