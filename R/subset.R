subset.mcarray <- function (x, sample = NULL, chain = NULL, ...) {
  
  if(is.null(sample) & is.null(chain))
    return (x)
  
  if(!(is.null(sample))) {
    if(!is.numeric(sample))
      stop("sample must be class integer")
    if(length(sample) == 0)
      stop("sample must at least one value")
    if(any(is.na(sample)))
      stop("sample must not contain missing values")
    if(max(sample) > nsamples(x) /nchains(x))
      stop("sample must not exceed the number of MCMC samples per chain")
    
    sample <- as.integer(sample)
    sample <- sort(unique(sample))
  } else {
    sample <- 1:(nsamples(x)/nchains(x))
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
  sample <- paste0("c(",paste0(sample,collapse = ","),")")
  chain <- paste0("c(",paste0(chain,collapse = ","),")")
  cmd <- paste0('x<-x[', commas ,sample, ',' ,chain, ',drop=F]')
  eval(parse(text = cmd))
  
  dimnames(x) <- dnames
  class(x) <- 'mcarray'
  
  return (x)
}

subset_mcarray <- function (x, sample = NULL, chain = NULL, ...) {
  stopifnot(is.mcarray(x))
  return (subset(x, sample = sample, chain = chain, ...))
}

subset.jagr_chains <- function (x, sample = NULL, chain = NULL) {
  
  samples(x) <- lapply(samples(x), FUN = subset_mcarray, sample=sample, chain=chain)
  
  jags(x) <- jags(x)[chain]
  
  return (x)
}

#' @title Subset a JAGS model
#'
#' @description
#' Subset a JAGS model x.  
#' 
#' @param x a jags_model x.
#' @param model_number an integer or character vector specifying the jags models to select. 
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
#' @param model_number an integer or character vector specifying the jags models to select. 
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
    if (model_number < 1 || model_number > nmodels(x)) {
      stop(paste("model_number must lie between 1 and the number of models (in this case",nmodels(x),")"))
    }
  } else {
    stop ("model_number must be an integer")
  }
  
  model_number <- as.integer(model_number)
  
  if (nmodels(x) == 1)
    return (x)
    
  analyses <- analyses(x)
  
  analyses(x) <- analyses[model_number]
  
  return (x)
}
