
#' @title Add JAGS objects
#'
#' @description
#' Adds two or more JAGS object of the same class.  
#' 
#' @param object a JAGS object.
#' @param object2 a second JAGS object to add to object.
#' @param ... additional JAGS objects to add to object.
#' @return a JAGS object of the original class
#' @seealso \code{\link{add_jags.jags_model}}, \code{\link{add_jags.jags_analysis}}, \code{\link{add_jags.jags_simulation}} and \code{\link{add_jags.jags_power_analysis}}
#' @export
add_jags <- function (object, object2, ...) {
  UseMethod("add_jags", object)
}

#' @method add_jags mcarray
add_jags.mcarray <- function (object, object2, ..., by = "iterations") {
  
  if(!inherits(by,"character"))
    stop("by must be class character")
  if(length(by) != 1)
    stop("by must be a character element")
  if(is.na(by))
    stop("by must not be a missing value")
  
  if(!by %in% c("iterations","chains"))
    stop("by must be 'iterations' or 'chains'")
  
  if(by == "chains") {

  if (!inherits (object2, "mcarray"))
    stop ("object2 should be class mcarray")
  if (niters (object) != niters (object2))
    stop ("object and object2 should have the same number of iterations")
  
  dimobj <- dim (object)
  dimobject2 <- dim (object2)
  dnames <- names(dim (object))
  
  if (!identical(dimobj[-length(dimobj)],dimobject2[-length(dimobject2)]))
    stop ("object and object2 should have the same dimensions (except chains)")
  
  class(object)<-"array"
  class(object2)<-"array"
  object <- abind (object,object2,along=length(dimobj))
  
  names(dim(object)) <- dnames
  class(object)<-"mcarray"
  } else if(by == "iterations") {
    
    if (!inherits (object, "mcarray"))
      stop ("object should be class mcarray")
    if (!inherits (object2, "mcarray"))
      stop ("object2 should be class mcarray")
    if (nchains (object) != nchains (object2))
      stop ("object and object2 should have the same number of chains")
    
    dimobj <- dim (object)
    dimiter <- dim (object2)
    dnames <- names(dim (object))
    
    if (!identical(dimobj[-(length(dimobj)-1)],dimiter[-(length(dimiter)-1)]))
      stop ("object and object2 should have the same dimensions (except iterations)")
    
    class(object)<-"array"
    class(object2)<-"array"
    object <- abind (object,object2,along=length(dimobj)-1)
    
    names(dim(object)) <- dnames
    class(object)<-"mcarray"
  }
  
  args <- list(...)
  nargs <- length(args)
  if (nargs > 0) {
    for (i in 1:nargs) {
      object[[i]] <- add_jags(object, args[[i]], by = by)
    }
  }
  return (object)
}

#' @method add_jags list
add_jags.list <- function (object, object2, ..., by = "iterations") {
 
  for (i in seq(along = object))
    object[[i]] <- add_jags(object[[i]], object2[[i]], by = by)
  
  args <- list(...)
  nargs <- length(args)
  if (nargs > 0) {
    for (i in 1:nargs) {
      object[[i]] <- add_jags(object, args[[i]], by = by)
    }
  }
  return (object)
}

#' @method add_jags jags_mcmc
add_jags.jags_mcmc <- function (object, object2, ...) {
  
  object$jags <- c(object$jags, object2$jags)
  
  object$mcmc <- add_jags (object$mcmc, object2$mcmc, by = "chains")
  
  object <- update_convergence_jags_mcmc(object)
  
  args <- list(...)
  nargs <- length(args)
  if (nargs > 0) {
    for (i in 1:nargs) {
      object <- add_jags(object, args[[i]], by = "chains")
    }
  }
  return (object)
}

add_jags_jags_mcmc <- function (object, object2) {
  
  if(!is.jags_mcmc(object))
    stop("object must be class jags_mcmc")

  if(!is.jags_mcmc(object2))
    stop("object2 must be class jags_mcmc")
  
  return (add_jags(object, object2))
  
  return (object)
}

#' @title Add JAGS models
#'
#' @description
#' Adds two or more JAGS models.  
#' 
#' @param object a jags_model.
#' @param object2 a second jags_model.
#' @param ... additional jags_models to add to object.
#' @return a jags_model object
#' @seealso \code{\link{add_jags}} and \code{\link{jags_model}}
#' @examples
#' model1 <- jags_model("
#' model { 
#'  bLambda ~ dlnorm(0, 10^-2) 
#'  for (i in 1:nrow) { 
#'    x[i]~dpois(bLambda) 
#'  } 
#'}")
#'
#' model2 <- jags_model("
#' model { 
#'  bLambda ~ dnorm(0, 10^-2) 
#'  sLambda ~ dunif(0, 5)
#'  for (i in 1:nrow) { 
#'    x[i] ~ dnorm(bLambda, sLambda^-2) 
#'  } 
#'}")
#'
#' models <- add_jags(model1, model2)
#' number_of_models(models)
#' @method add_jags jags_model
#' @export 
add_jags.jags_model <- function (object, object2, ...)
{
  object$models <- c(object$models,object2$models) 
  object$derived_code <- c(object$derived_code,object2$derived_code) 
  object$random_effects <- c(object$random_effects,object2$random_effects) 
  
  args <- list(...)
  nargs <- length(args)
  if (nargs > 0) {
    for (i in 1:nargs) {
      object <- add_jags(object, args[[i]])
    }
  }
  return (object)
}

#' @method add_jags jags_analysis
#' @export 
add_jags.jags_analysis <- function (object, object2, ..., mode = "current")
{
  stop("not yet implemented")
  return (object)
}
    
#' @method add_jags jags_simulation
#' @export 
add_jags.jags_simulation <- function (object, object2, ..., mode = "current")
{
  if(!is.jags_simulation(object))
    stop("object should be of class jags_simulation")

  if(!is.jags_simulation(object2))
    stop("object2 should be of class jags_simulation")
  
  if(!identical(object$data_model,object2$data_model))
    stop("objects must have identical data_models")
  
  if(any(!colnames(object$values) %in% colnames(object2$values)))
    stop("objects must have values with the same names")
  
  if(any(!colnames(object2$values) %in% colnames(object$values)))
    stop("objects must have values with the same names")
  
  object2$values <- subset(object2$values, select = colnames(object$values))
  
  diff <- abs(nrep(object) - nrep(object2))
  
  if(diff != 0) {
    if (nrep(object) > nrep(object2)) {
      object2 <- update_jags(object2, nrep = diff, mode = mode)
    } else {
      object <- update_jags(object, nrep = diff, mode = mode)
    }
  }
  
  object$values <- rbind(object$values, object2$values)
  object$data <- c(object$data,object2$data)
  
  args <- list(...)
  nargs <- length(args)
  if (nargs > 0) {
    for (i in 1:nargs) {
      object <- add_jags(object, args[[i]])
    }
  }
  return (object)
}

#' @method add_jags jags_power_analysis
#' @export 
add_jags.jags_power_analysis <- function (object, object2, ..., mode = "current")
{
  stop("not yet implemented")
  return (object)
}
