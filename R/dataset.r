
dataset <- function (object, ...) {
  UseMethod("dataset", object)
}

#' @title Get data from a JAGS analysis
#'
#' @description
#' By default returns the input data set from a JAGS analysis 
#' or if \code{base = TRUE} generates a data set with one row specify the #' \emph{base} values for each of the variables. For further information
#' on the use of base values see the \code{predict.jags_analysis} function. 
#' 
#' @param object a jags_analysis object
#' @param base a logical scalar indicating whether to return the base values
#' @return a data frame of the data used in the analysis 
#' or the corresponding base values
#' @seealso \link{jags_analysis} and \link{predict.jags_analysis}
#' @method dataset jags_simulation
dataset.jags_analysis <- function (object, base = FALSE) {
  
  if(!is.jags_analysis(object))
    stop("object should be of class jags_analysis")
  
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

#' @method dataset jags_simulation
dataset.jags_simulation <- function (object, rep = 1, value = 1, variables = NULL) {
  
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
  
  if(!(is.null(variables))) {
    if(!is.character(variables))
      stop("variables must be class character")
    if(length(variables) == 0)
      stop("variables must at least one value")
    if(any(is.na(variables)))
      stop("variables must not contain missing values")
    
    variables <- sort(unique(variables))
    
    vnames <- names(object$simulated[[1]][[1]])
    
    if(any(!variables %in% vnames))
      stop("unrecognised variables")
    
  } else {
    variables <- names(object$simulated[[1]][[1]])
  }
  
  data <- object$simulated[value]
  
  for (i in 1:length(data)) {
    data[[i]] <- data[[i]][rep]
    for (j in rep) {
      data[[i]][[j]] <- data[[i]][[j]][variables]
    }
  }
  
  return (data)
}
