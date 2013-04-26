
parm<- function (object, ...) {
  UseMethod("parm", object)
}

parm.jagr_analysis <- function (object, parm = "fixed") {
  
  if(!is.jagr_analysis(object))
    stop("object must be class jagr_analysis")
  
  if(!is.character(parm)) {
    stop("parm must be class character")
  }
    
  model <- object$model
  
  parm <- unique(parm)
  
  pars <- model$monitor
  
  if("all" %in% parm || all(c("fixed","random") %in% parm)) {
    return (pars)
  }
  
  if ("fixed" %in% parm) {
    add <- pars[!pars %in% names(model$random)]
  } else if ("random" %in% parm) {
    add <- pars[pars %in% names(model$random)]
    if (is.null(add)) {
      warning("there are no monitored random parameters")
      add <- "deviance"
    }
  }
  pars <- pars[pars %in% parm]
  pars <- unique(c(pars,add))  
  
  return (pars)
}

parm.jags_analysis <- function (object, parm = "fixed", model_number = 1) {

  if(!is.jags_analysis(object))
    stop("object must be class jags_analysis")
  
  if(!is.character(parm)) {
    stop("parm must be class character")
  }
  
  object <- subset(object, model_number = model_number)
  
  return (parm (object$analyses[[1]], parm = parm))
}
