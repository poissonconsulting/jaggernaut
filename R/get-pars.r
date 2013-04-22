
get_pars <- function (object, type = "fixed") {
  if(!is.jags_model(object))
    stop("object must be of class jags_model")
  
  if (!type %in% c("fixed","random","all"))
    stop ("type should be fixed, random or all")
  
  pars <- object$monitor
  if (type == "fixed") {
    pars <- pars[!pars %in% names(object$random)]
  } else if (type == "random") {
    random <- names(object$random)
    if (is.null(random)) {
      warning("there are no random monitored parameters - returning deviance")
      pars <- "deviance"
    } else {
      pars <- random
    }
  }
  return (pars)
}
