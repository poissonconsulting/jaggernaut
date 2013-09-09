#' @export
subset_jags <- function (x, ...) {
  UseMethod("subset_jags", x)
}

#' @method subset_jags jags_analysis
#' @export 
subset_jags.jags_analysis <- function (x, model_number = 0, ...) {   
  
  if (!is.jags_analysis(x)) {
    stop("x must be a jags_analysis")
  }
  
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
  newObject$analyses <- list()
  if(model_number == 0) {
    newObject$analyses[[1]] <- x$analyses[[rownames(x$dic)[1]]]
    
  } else {
    newObject$analyses[[1]] <- x$analyses[[model_number]]
  }
  newObject$dic <- x$dic[rownames(x$dic) == paste0("Model",model_number),,drop=T]
  newObject$n.model <- 1
  
  class(newObject) <- "jags_analysis"
  
  return (newObject)
}

#' @method subset_jags jags_simulation
#' @export 
subset_jags.jags_simulation <- function (x, rep = 1, value = 1, ...) {   
  if(!is.jags_simulation(x))
    stop("x should be of class jags_simulation")
  
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

#' @method subset_jags jags_power_analysis
#' @export 
subset_jags.jags_power_analysis <- function (x, rep = 1, value = 1, ...) {   
  stop("not yet implemented")
  
  return (x)
}
