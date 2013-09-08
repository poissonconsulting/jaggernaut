
#' @export
data_jags <- function (object, ...) {
  UseMethod("data_jags", object1)
}

#' @method data_jags jags_analysis
#' @export 
data_jags.jags_analysis <- function (object)
{
  if(!is.jags_analysis(object))
    stop("object should be of class jags_analysis")
  
  data <- dataset(object, base = TRUE)
  
  return (data)
}

#' @method data_jags jags_simulation
#' @export 
data_jags.jags_simulation <- function (object, value = 1, rep = 1)
{
  if(!is.jags_simulation(object))
    stop("object should be of class jags_simulation")
  
  if(!(is.null(value))) {
    if(!is.numeric(value))
      stop("value must be class integer")
    if(length(value) == 0)
      stop("value must at least one value")
    if(any(is.na(value)))
      stop("value must not contain missing values")
    if(max(value) > object$nvalues)
      stop("value must be less than number of values")
    
    value <- as.integer(value)
    value <- sort(unique(value))
  } else {
    value <- 1:object$nvalues
  }
  
  if(!(is.null(rep))) {
    if(!is.numeric(rep))
      stop("rep must be class integer")
    if(length(rep) == 0)
      stop("rep must at least one value")
    if(any(is.na(rep)))
      stop("rep must not contain missing values")
    if(max(rep) > object$nrep)
      stop("rep must be less than number of values")
    
    rep <- as.integer(rep)
    rep <- sort(unique(rep))
  } else {
    rep <- 1:object$nrep
  }
  
  data <- dataset(object, rep = rep, value = value)
  
  if(length(value) == 1) {
    data <- data[[value]]
    if (length(rep) == 1)
      data <- data[[rep]]
  }
  
  return (data)
}
