
#' @title Subset a JAGS object
#'
#' @description
#' Subsets a JAGS object.  
#' 
#' @param object a JAGS object.
#' @param ... other arguments.
#' @return an object of the same JAGS class.
#' @seealso \code{\link{subset_jags.jags_analysis}},
#' \code{\link{subset_jags.jags_simulation}} and \code{\link{subset_jags.jags_power_analysis}}
#' @export
subset_jags <- function (object, ...) {
  UseMethod("subset_jags", object)
}

#' @title Subset a JAGS model
#'
#' @description
#' Subset a JAGS model object.  
#' 
#' @param object a jags_model object.
#' @param models an integer element indicating the model(s) to select.
#' @param ... other arguments passed to generic function.
#' @return the subsetted jags_analysis object
#' @seealso \code{\link{subset_jags}} and \code{\link{jags_analysis}}
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
#' models <- add_jags(model1, model2, model1)
#' data <- data.frame(x = rpois(100,10))
#' 
#' analysis <- jags_analysis (models, data, mode = "demo")
#' 
#' summary(analysis)
#' summary(subset_jags(analysis))
#' summary(subset_jags(analysis, model_number = 2))
#' 
#' @method subset_jags jags_model
#' @export 
subset_jags.jags_model <- function (object, model = NULL, ...) {   
  
  if(is.null(model))
    return (object)
  
  if (!is.numeric(model)) 
    stop("model must be an integer vector")

  if (length(model) == 0)
    stop("model must have at least one value")

  if (any(is.na(model)))
    stop("model must not include missing values")
  
  model <- as.integer(model)
  
  if (max(model) > nmodel(object))
    stop("model must be less than the number of models in object")  
  
  if (min(model) < 1)
    stop("model must be less positive")  
  
  object$models <- object$models[model]
  object$nmodel <- length(object$models)
  
  return (object)
}

#' @title Subset a JAGS analysis
#'
#' @description
#' Subset a JAGS analysis object.  
#' 
#' @param object a jags_analysis object.
#' @param model_number an integer element indicating the model to select.
#' @param ... other arguments passed to generic function.
#' @return the subsetted jags_analysis object
#' @seealso \code{\link{subset_jags}} and \code{\link{jags_analysis}}
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
#' models <- add_jags(model1, model2, model1)
#' data <- data.frame(x = rpois(100,10))
#' 
#' analysis <- jags_analysis (models, data, mode = "demo")
#' 
#' summary(analysis)
#' summary(subset_jags(analysis))
#' summary(subset_jags(analysis, model_number = 2))
#' 
#' @method subset_jags jags_analysis
#' @export 
subset_jags.jags_analysis <- function (object, model = NULL, ...) {   
  
  if(is.null(model))
    return (object)
    
  x <- object
  rm(object)
  
  model_number <- model
  rm(model)
  
  if (is.numeric(model_number)) {
    if (length(model_number) != 1) {
      stop("model_number must be a single value")
    }
    if (model_number < 0 || model_number > nmodel(x)) {
      stop(paste("model_number must lie between 0 and the number of models (in this case",nmodel(x),")"))
    }
  } else {
    stop ("model_number must be an integer")
  }
  
  model_number <- as.integer(model_number)
  
  if (nmodel(x) == 1)
    return (x)
  
  newObject <- list()
  newObject$data <- x$data
  newObject$analyses <- list()
  if(model_number == 0) {
    newObject$analyses[[1]] <- x$analyses[[rownames(x$dic)[1]]]
    
  } else {
    newObject$analyses[[1]] <- x$analyses[[model_number]]
  }
  newObject$dic <- x$dic[rownames(x$dic) == paste0("Model",model_number),,drop=T]
  newObject$n.model <- 1
  newObject$rhat <- x$rhat
  
  class(newObject) <- "jags_analysis"
  
  return (newObject)
}

#' @title Subset a JAGS simulation
#'
#' @description
#' Subset a JAGS simulation object.  
#' 
#' @param object a jags_simulation object.
#' @param value an integer vector indicating the rows in values to select.
#' @param rep an integer vector indicating the replicates to select.
#' @param ... other arguments passed to generic function.
#' @return the subsetted JAGS simulation object.
#' @seealso \code{\link{subset_jags}} and \code{\link{jags_simulation}}
#' #' @examples
#' 
#' data_model <- jags_data_model("
#' data { 
#'  for (i in 1:nx) { 
#'    x[i] ~ dpois(bIntercept) 
#'    for (j in 1:nx) {
#'      y[i,j] ~ dpois(bIntercept) 
#'    }
#'  } 
#'  z <- bIntercept
#'}    
#' ")
#'
#' values <- data.frame(nx = c(1,10), bIntercept = c(5,10))
#' 
#' simulation <- jags_simulation (data_model, values, nrep = 5, mode = "test")
#' 
#' subset_jags(simulation)
#' subset_jags(simulation, value = 1, rep = NULL)
#' subset_jags(simulation, value = NULL, rep = 1)
#' subset_jags(simulation, value = 1, rep = 1)
#' @method subset_jags jags_simulation
#' @export 
subset_jags.jags_simulation <- function (object, value = NULL, rep = NULL, ...) {   

  if(is.null(rep) & is.null(value))
    return (object)
  
  x <- object
  rm(object)
  
  if(!(is.null(value))) {
    if(!is.numeric(value))
      stop("value must be class integer")
    if(length(value) == 0)
      stop("value must at least one value")
    if(any(is.na(value)))
      stop("value must not contain missing values")
    if(max(value) > x$nvalues)
      stop("value must be less than number of values")
    
    value <- as.integer(value)
    value <- sort(unique(value))
  } else {
    value <- 1:x$nvalues
  }
  
  if(!(is.null(rep))) {
    if(!is.numeric(rep))
      stop("rep must be class integer")
    if(length(rep) == 0)
      stop("rep must at least one value")
    if(any(is.na(rep)))
      stop("rep must not contain missing values")
    if(max(rep) > x$nrep)
      stop("rep must be less than number of values")
    
    rep <- as.integer(rep)
    rep <- sort(unique(rep))
  } else {
    rep <- 1:x$nrep
  }
  
  x$simulated <- data_jags(x, value = value, rep = rep)
  x$values <- x$values[value,,drop = FALSE]
  x$nvalues <- nrow(x$values)
  x$nrep <- length(rep)
  
  return (x)
}

subset_jags.jags_power_analysis <- function (object, model = NULL, value = NULL, rep = NULL, ...) {
  
  if(is.null(model) & is.null(rep) & is.null(value))
    return (object)
  
  stop("not yet implemented")
  
  return (object)
}
