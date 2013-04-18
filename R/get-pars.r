
get_pars <- function (object, type = "fixed") {
  if(!is.jmodel(object))
    stop("object must be of class jmodel")
  
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
