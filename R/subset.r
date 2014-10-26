subset.mcarray <- function (x, sim = NULL, chain = NULL, ...) {
  
  if(is.null(sim) & is.null(chain))
    return (x)
  
  if(!(is.null(sim))) {
    if(!is.numeric(sim))
      stop("sim must be class integer")
    if(length(sim) == 0)
      stop("sim must at least one value")
    if(any(is.na(sim)))
      stop("sim must not contain missing values")
    if(max(sim) > nsims(x) /nchains(x))
      stop("sim must not exceed the number of MCMC simulations per chain")
    
    sim <- as.integer(sim)
    sim <- sort(unique(sim))
  } else {
    sim <- 1:(nsims(x)/nchains(x))
  }
  
  if(!(is.null(chain))) {
    if(!is.numeric(chain))
      stop("chain must be class integer")
    if(length(chain) == 0)
      stop("chain must at least one value")
    if(any(is.na(chain)))
      stop("chain must not contain missing values")
    if(max(chain) > nchains(x))
      stop("chain must not exceed the number of chains")
    
    chain <- as.integer(chain)
    chain <- sort(unique(chain))
  } else {
    chain <- 1:nchains(x)
  }
  
  ndim <- length (dim(x))
  dnames <- dimnames(dim(x))
  
  commas <- paste0(rep(',',ndim - 2),collapse = "")   
  sim <- paste0("c(",paste0(sim,collapse = ","),")")
  chain <- paste0("c(",paste0(chain,collapse = ","),")")
  cmd <- paste0('x<-x[', commas ,sim, ',' ,chain, ',drop=F]')
  eval(parse(text = cmd))
  
  dimnames(x) <- dnames
  class(x) <- 'mcarray'
  
  return (x)
}

subset_mcarray <- function (x, sim = NULL, chain = NULL, ...) {
  stopifnot(is.mcarray(x))
  return (subset(x, sim = sim, chain = chain, ...))
}

subset.jagr_chains <- function (x, sim = NULL, chain = NULL) {
  
  samples(x) <- lapply(samples(x), FUN = subset_mcarray, sim=sim, chain=chain)
  
  jags(x) <- jags(x)[chain]
  
  return (x)
}

#' @title Subset a JAGS model
#'
#' @description
#' Subset a JAGS model x.  
#' 
#' @param x a jags_model x.
#' @param model_number an integer element indicating the model(s) to select.
#' @param ... other arguments passed to generic function.
#' @return the subsetted jags_analysis x
#' @seealso \code{\link{subset}} and \code{\link{jags_analysis}}
#' @method subset jags_model
#' @export 
subset.jags_model <- function (x, model_number, ...) {   
  
  if (!is.numeric(model_number)) 
    stop("model_number must be an integer vector")
  
  model_number <- na.omit(model_number)
  
  if (length(model_number) == 0)
    stop("model_number must have at least one non-missing value")
  
  model_number <- as.integer(model_number)
  
  if (max(model_number) > nmodels(x))
    stop("model must be less than the number of models in x")  
  
  if (min(model_number) < 1)
    stop("model must be positive")  
  
  models <- models(x)
  models <- models[model_number] 
  models(x) <- models
  
  return (x)
}

#' @title Subset a JAGS analysis
#'
#' @description
#' Subset a JAGS analysis x.  
#' 
#' @param x a jags_analysis x.
#' @param model_number an integer element indicating the model(s) to select.
#' @param ... other arguments passed to generic function.
#' @return the subsetted jags_analysis x
#' @seealso \code{\link{subset}} and \code{\link{jags_analysis}} 
#' @method subset jags_analysis
#' @export 
subset.jags_analysis <- function (x, model_number = NULL, ...) {   
  
  if(is.null(model_number))
    return (x)
  
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
  
  if(model_number == 0)
    model_number <- as.integer(substr(rownames(dic_jags(x))[1],6,8))
  
  analyses <- analyses(x)
  
  analyses(x) <- analyses[model_number]
  
  return (x)
}
