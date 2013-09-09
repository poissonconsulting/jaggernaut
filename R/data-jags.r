
#' @title Get dataset(s) from a JAGS object
#'
#' @description
#' Gets the dataset(s) from a JAGS object.  
#' 
#' @param object a object.
#' @param ... other arguments.
#' @return a data.frame or list(s) of the data
#' @seealso \code{\link{data_jags.jags_data_model}},  \code{\link{data_jags.jags_analysis}},
#' \code{\link{data_jags.jags_simulation}} and \code{\link{data_jags.jags_power_analysis}}
#' @export
data_jags <- function (object, ...) {
  UseMethod("data_jags", object)
}

#' @title Get dataset from a JAGS data model
#'
#' @description
#' Simulates a dataset from a JAGS data model.  
#' 
#' @param object a JAGS data model.
#' @param values a data.frame with a single row of data indicating the values for the simulation.
#' @param ... other arguments passed to generic function.
#' @return the simulated dataset in list form.
#' @seealso \code{\link{data_jags}} and \code{\link{jags_data_model}}
#' @examples
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
#' values <- data.frame(nx = 10, bIntercept = 5)
#' 
#' data_jags(data_model, values)
#' @method data_jags jags_data_model
#' @export
data_jags.jags_data_model <- function (object, values, ...)
{ 
  if (!is.data.frame(values))
    stop("values must be a data.frame")

  if (nrow(values) != 1) {
    warning("only using the first row of values")
    values <- values[1, , drop = FALSE]
  }
  simu <- jagr_simulation(model = object, 
                            data = values, 
                            quiet = TRUE)
  
  est <- calc_estimates(simu)
  
  est <- est[rownames(est) != "deviance",]
  
  data <- extract_estimates(est)[["estimate"]]

  return (data)
}

#' @title Get dataset from a JAGS analysis
#'
#' @description
#' Gets the dataset from a JAGS analysis object.  
#' 
#' @param object a jags_analysis object.
#' @param based a logical element indicating xx.
#' @param ... other arguments passed to generic function.
#' @return a data.frame or list of the data used in the analysis
#' @seealso \code{\link{data_jags}} and \code{\link{jags_analysis}}
#' @examples
#' model <- jags_model("
#' model { 
#'  bLambda ~ dlnorm(0, 10^-2) 
#'  for (i in 1:nrow) { 
#'    x[i]~dpois(bLambda) 
#'  } 
#'}")
#'
#' data <- data.frame(x = rpois(100,10))
#' 
#' analysis <- jags_analysis (model, data, mode = "demo")
#' 
#' data_jags(analysis)
#' data_jags(analysis, base = TRUE)
#' 
#' @method data_jags jags_analysis
#' @export
data_jags.jags_analysis <- function (object, base = FALSE, ...)
{   
  if(base && is_data_list(object$analyses[[1]]))
    stop("if data is a data list base must be FALSE")
  
  object <- object$analyses[[1]]
  
  if(!base) {
    return (object$data)
  }
  
  data <- generate_data (object$data)
  if (!is.null(object$block$select))
    data <- subset (data, select = names_select(object$block$select))
  
  return (data)
}

#' @title Get dataset(s) from a JAGS simulation
#'
#' @description
#' Gets the dataset(s) from a JAGS simulation object.  
#' 
#' @param object a jags_simulation object.
#' @param value an integer vector indicating the rows in values to select.
#' @param rep an integer vector indicating the replicates to select.
#' @param ... other arguments passed to generic function.
#' @return a list or lists of the simulated data
#' @seealso \code{\link{data_jags}} and \code{\link{jags_simulation}}
#' @method data_jags jags_simulation
#' @export
data_jags.jags_simulation <- function (object, value = 1, rep = 1, ...)
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
  
  data <- object$simulated[value]
  
  for (i in 1:length(data)) {
    data[[i]] <- data[[i]][rep]
    for (j in rep) {
      data[[i]][[j]] <- data[[i]][[j]]
    }
  }
  
  if(length(value) == 1) {
    data <- data[[value]]
    if (length(rep) == 1)
      data <- data[[rep]]
  }
  
  return (data)
}

data_jags.jags_power_analysis <- function (object, value = 1, rep = 1, ...)
{  
  stop("not yet implemented")
  
  return (data)
}
