
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
add_jags.jags_model <- function (object, object2, ..., mode = "current")
{
  object$models <- c(object$models,object2$models) 
  object$nmodel <- object$nmodel + object2$nmodel
  
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
  
  diff <- abs(object$nrep - object2$nrep)
  
  if(diff != 0) {
    if (object$nrep > object2$nrep) {
      object2 <- update_jags(object2, nrep = diff, mode = mode)
    } else {
      object <- update_jags(object, nrep = diff, mode = mode)
    }
  }
  
  object$values <- rbind(object$values, object2$values)
  object$nvalues <- nrow(object$values)
  object$simulated <- c(object$simulated,object2$simulated)
  
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
