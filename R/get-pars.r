
get_pars <- function (object, type = "fixed") {
  if(!is.jmodel(object))
    stop("object must be of class jmodel")
  
  if (!(type %in% c("fixed","random","all") || is.null(type)))
    stop ("type should be fixed, random, all or NULL")
  
  if(is.null(type))
    type <- "all"
  
  pars <- object$monitor
  if (type == "fixed") {
    pars <- pars[!pars %in% names(object$random)]
  } else if (type == "random") {
    pars <- names(object$random)
  }
  return (pars)
}