
#' @title Number of models in a JAGS object
#'
#' @description 
#' Gets the number of models in a JAGS object
#'   
#' @param object a JAGS object
#' @return an integer element indicating the number of models in object
#' @aliases nmodel
#' @seealso \code{\link{number_of_models.jags_model}} and \code{\link{number_of_models.jags_analysis}}
#' @export
number_of_models <- function (object, ...) {
  UseMethod("number_of_models", object)
}

#' @export
nmodel <- function (object, ...) {
  UseMethod("number_of_models", object)
}

#' @title Number of models in a JAGS model
#'
#' @description 
#' Gets the number of models in a JAGS model object
#'   
#' @param object a jags_model object
#' @param ... other arguments passed to generic function.
#' @seealso \code{\link{number_of_models}} and \code{\link{jags_model}} 
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
#' model <- add_jags(model1, model2)
#' number_of_models(model)
#' @method number_of_models jags_model
#' @export
number_of_models.jags_model <- function (object, ...) {
  return (as.integer(object$nmodel))
}

#' @title Number of models in a JAGS analysis
#'
#' @description 
#' Gets the number of models in a JAGS analysis object.
#'   
#' @param object a jags_analysis object
#' @param ... other arguments passed to generic function.
#' @seealso \code{\link{number_of_models}} and \code{\link{jags_analysis}} 
#' @method number_of_models jags_analysis
#' @export
number_of_models.jags_analysis <- function (object, ...) {
  
  return (as.integer(object$n.model))
}
