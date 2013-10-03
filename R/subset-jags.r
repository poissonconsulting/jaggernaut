
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
#' @param model_number an integer element indicating the model(s) to select.
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
subset_jags.jags_model <- function (object, model_number = NULL, ...) {   
  
  model <- model_number
  
  if(is.null(model))
    return (object)
  
  if (!is.numeric(model)) 
    stop("model must be an integer vector")

  if (length(model) == 0)
    stop("model must have at least one value")

  if (any(is.na(model)))
    stop("model must not include missing values")
  
  model <- as.integer(model)
  
  if (max(model) > nmodels(object))
    stop("model must be less than the number of models in object")  
  
  if (min(model) < 1)
    stop("model must be less positive")  
  
  object$models <- object$models[model]
  object$derived_code <- object$derived_code[model]
  object$random_effects <- object$random_effects[model]
  
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
subset_jags.jags_analysis <- function (object, model_number = NULL, ...) {   
    
  if(is.null(model_number))
    return (object)
    
  x <- object
  rm(object)
  
  if (is.numeric(model_number)) {
    if (length(model_number) != 1) {
      stop("model_number must be a single value")
    }
    if (model_number < 0 || model_number > nmodels(x)) {
      stop(paste("model_number must lie between 0 and the number of models (in this case",nmodels(x),")"))
    }
  } else {
    stop ("model_number must be an integer")
  }
  
  model_number <- as.integer(model_number)
  
  if (nmodels(x) == 1)
    return (x)
  
  newObject <- list()
  newObject$data <- x$data
  newObject$analyses <- list()
  newObject$derived_code <- list()
  newObject$random_effects <- list()
  
  if(model_number == 0)
    model_number <- as.integer(substr(rownames(x$dic)[1],6,8))

  newObject$analyses[[1]] <- x$analyses[[model_number]]
  newObject$derived_code[[1]] <- x$derived_code[[model_number]]
  newObject$random_effects[[1]] <- x$random_effects[[model_number]]
  
  dic <- t(sapply(newObject$analyses,DIC_jagr_analysis))
  rownames(dic) <- paste0("Model",1:nrow(dic))  
  newObject$dic <- dic[order(dic[,"DIC",drop=T]),]    
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
  
  if(!(is.null(value))) {
    if(!is.numeric(value))
      stop("value must be class integer")
    if(length(value) == 0)
      stop("value must at least one value")
    if(any(is.na(value)))
      stop("value must not contain missing values")
    if(max(value) > nvalue(object))
      stop("value must be less than number of values")
    
    value <- as.integer(value)
    value <- sort(unique(value))
  } else {
    value <- 1:nvalue(object)
  }
  
  if(!(is.null(rep))) {
    if(!is.numeric(rep))
      stop("rep must be class integer")
    if(length(rep) == 0)
      stop("rep must at least one value")
    if(any(is.na(rep)))
      stop("rep must not contain missing values")
    if(max(rep) > nrep(object))
      stop("rep must be less than number of replicates")
    
    rep <- as.integer(rep)
    rep <- sort(unique(rep))
  } else {
    rep <- 1:nrep(object)
  }
  
  data <- object$data[value]
  
  for (i in 1:length(data)) {
    data[[i]] <- data[[i]][rep]
    for (j in rep) {
      data[[i]][[j]] <- data[[i]][[j]]
    }
  }

  object$data <- data
  object$values <- object$values[value,,drop = FALSE]
  
  return (object)
}

subset_jags.jags_power_analysis <- function (object, model_number = NULL, value = NULL, rep = NULL, ...) {
  
  if(is.null(model) & is.null(rep) & is.null(value))
    return (object)
  
  stop("not yet implemented")
  
  return (object)
}
