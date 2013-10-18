
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

subset_jags.mcarray <- function (object, sim = NULL, chain = NULL, ...) {

  if(is.null(sim) & is.null(chain))
    return (object)
  
  if(!(is.null(sim))) {
    if(!is.numeric(sim))
      stop("sim must be class integer")
    if(length(sim) == 0)
      stop("sim must at least one value")
    if(any(is.na(sim)))
      stop("sim must not contain missing values")
    if(max(sim) > nsims(object) /nchains(object))
      stop("sim must not exceed the number of MCMC simulations per chain")
    
    sim <- as.integer(sim)
    sim <- sort(unique(sim))
  } else {
    sim <- 1:(nsims(object)/nchains(object))
  }
  
  if(!(is.null(chain))) {
    if(!is.numeric(chain))
      stop("chain must be class integer")
    if(length(chain) == 0)
      stop("chain must at least one value")
    if(any(is.na(chain)))
      stop("chain must not contain missing values")
    if(max(chain) > nchains(object))
      stop("chain must not exceed the number of chains")
    
    chain <- as.integer(chain)
    chain <- sort(unique(chain))
  } else {
    chain <- 1:nchains(object)
  }
  
  ndim <- length (dim(object))
  dnames <- dimnames(dim(object))
  
  commas <- paste0(rep(',',ndim - 2),collapse = "")   
  sim <- paste0("c(",paste0(sim,collapse = ","),")")
  chain <- paste0("c(",paste0(chain,collapse = ","),")")
  cmd <- paste0('object<-object[', commas ,sim, ',' ,chain, ',drop=F]')
  eval(parse(text = cmd))
  
  dimnames(object) <- dnames
  class(object) <- 'mcarray'
  
  return (object)
}

subset_jags_mcarray <- function (object, sim = NULL, chain = NULL, ...) {
  stopifnot(is.mcarray(object))
  return (subset_jags(object, sim = sim, chain = chain, ...))
}
  
subset_jags.jagr_chains <- function (object, sim = NULL, chain = NULL) {

  samples(object) <- lapply(samples(object), FUN = subset_jags_mcarray, sim=sim, chain=chain)
  
  jags(object) <- jags(object)[chain]
    
  return (object)
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
#' @method subset_jags jags_model
#' @export 
subset_jags.jags_model <- function (object, model_number, ...) {   
  
  if (!is.numeric(model_number)) 
    stop("model_number must be an integer vector")

  model_number <- na.omit(model_number)
  
  if (length(model_number) == 0)
    stop("model_number must have at least one non-missing value")
  
  model_number <- as.integer(model_number)
  
  if (max(model_number) > nmodels(object))
    stop("model must be less than the number of models in object")  
  
  if (min(model_number) < 1)
    stop("model must be positive")  
  
  models <- models(object)
  models <- models[model_number] 
  models(object) <- models
  
  return (object)
}

#' @title Subset a JAGS analysis
#'
#' @description
#' Subset a JAGS analysis object.  
#' 
#' @param object a jags_analysis object.
#' @param model_number an integer element indicating the model(s) to select.
#' @param ... other arguments passed to generic function.
#' @return the subsetted jags_analysis object
#' @seealso \code{\link{subset_jags}} and \code{\link{jags_analysis}} 
#' @method subset_jags jags_analysis
#' @export 
subset_jags.jags_analysis <- function (object, model_number = NULL, ...) {   
    
  if(is.null(model_number))
    return (object)
  
  if (is.numeric(model_number)) {
    if (length(model_number) != 1) {
      stop("model_number must be a single value")
    }
    if (model_number < 0 || model_number > nmodels(object)) {
      stop(paste("model_number must lie between 0 and the number of models (in this case",nmodels(object),")"))
    }
  } else {
    stop ("model_number must be an integer")
  }
  
  model_number <- as.integer(model_number)
  
  if (nmodels(object) == 1)
    return (object)

  if(model_number == 0)
    model_number <- as.integer(substr(rownames(dic_jags(object))[1],6,8))
  
  analyses <- analyses(object)
  
  analyses(object) <- analyses[model_number]
  
  return (object)
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
    if(max(value) > nvalues(object))
      stop("value must be less than number of values")
    
    value <- as.integer(value)
    value <- sort(unique(value))
  } else {
    value <- 1:nvalues(object)
  }
  
  if(!(is.null(rep))) {
    if(!is.numeric(rep))
      stop("rep must be class integer")
    if(length(rep) == 0)
      stop("rep must at least one value")
    if(any(is.na(rep)))
      stop("rep must not contain missing values")
    if(max(rep) > nreps(object))
      stop("rep must be less than number of replicates")
    
    rep <- as.integer(rep)
    rep <- sort(unique(rep))
  } else {
    rep <- 1:nreps(object)
  }
  
  values(object) <- values(object)[value,,drop = FALSE]
  
  data <- data_jags(object)[value]
  
  for (i in 1:length(data))
    data[[i]] <- data[[i]][rep]

  data_jags(object) <- data
  
  return (object)
}

subset_jags.jags_power_analysis <- function (object, model_number = NULL, value = NULL, rep = NULL, ...) {
  
  if(is.null(model) & is.null(rep) & is.null(value))
    return (object)
  
  stop("not yet implemented")
  
  return (object)
}
